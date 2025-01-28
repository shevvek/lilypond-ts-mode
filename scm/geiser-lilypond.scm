;;; geiser-lilypond.scm --- Lilypond layer for Geiser Guile

;; Copyright (c) 2025 Saul James Tobin

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(define-module (geiser-lilypond)
  #:export (ly:all-context-names
            ly:all-grob-names
            ly:all-translator-names
            ly:list-builtin-constants
            ly:music-word?
            keywords-of-type
            ly:grob-property-completions
            ly:accepts-maybe-property-path?)
  #:use-module (ice-9 and-let-star)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 session)
  #:use-module (ice-9 documentation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (system vm program)
  #:use-module (lily)
  #:use-module (lily curried-definitions))

(define-private gdoc (resolve-module '(geiser doc)))

(ly:load "document-identifiers")

(define (ly:primitive-args proc)
  (and-let* (((program? proc))
             (doc (object-documentation proc))
             (arities (program-arguments-alist proc))
             (arg-names (map (lambda (m)
                               (string->symbol
                                (match:substring m 1)))
                             (list-matches "SCM ([^ ,()]+)"
                                           doc)))
             (req-count (length (assq-ref arities 'required)))
             (opt-count (length (assq-ref arities 'optional)))
             (kw-count (length (assq-ref arities 'keyword)))
             (rest-count (if (assq-ref arities 'rest) 1 0))
             ((= (length arg-names)
                 (+ req-count opt-count kw-count rest-count))))
    (let*-values
        (((req-args rest) (split-at arg-names req-count))
         ((opt-args rest) (split-at rest opt-count))
         ((kw-args rest) (split-at rest kw-count))
         ((rest) (and (pair? rest) (car rest)))
         )
      `(((required . ,req-args)
         (optional . ,opt-args)
         (keyword . ,kw-args)
         (rest . ,rest))))))

(define (ly:music-function-arguments mf)
  (and-let* (((ly:music-function? mf))
             (sig (ly:music-function-signature mf))
             (func (ly:music-function-extract mf))
             (arg-names (syntax-function-procedure-arguments func))
             ((= (length arg-names)
                 (length (cdr sig))))
             (return-type (procedure-name (if (pair? (car sig))
                                              (caar sig)
                                              (car sig))))
             (arg-spec (map (lambda (name type)
                              (if (pair? type)
                                  `(,(procedure-name (car type))
                                    ,name
                                    ,(cdr type))
                                  `(,(procedure-name type)
                                    ,name)))
                            arg-names (cdr sig))))
    `((type . ,return-type)
      (arguments . ,arg-spec))))

(let ((old-args (module-ref gdoc 'arguments))
      (old-sig (module-ref gdoc 'signature))
      (old-docstring (module-ref gdoc 'docstring))
      (maybe-texinfo (module-ref gdoc 'try-texinfo->plain-text)))
  (module-set! gdoc 'arguments
               (lambda (proc)
                 (or (old-args proc)
                     (ly:music-function-arguments proc)
                     (ly:primitive-args proc))))
  (module-set! gdoc 'signature
               (lambda* (id args-list #:optional (detail #t))
                 (let ((ftype (assq-ref args-list 'type)))
                   (if ftype
                       `(,id ("args" ,@(assq-ref args-list 'arguments))
                             ("type" . ,ftype))
                       (old-sig id args-list detail)))))
  (module-set! gdoc 'docstring
               (lambda (sym obj)
                 (if (ly:music-function? obj)
                     (maybe-texinfo
                      (document-music-function (cons sym obj)))
                     (old-docstring sym obj)))))

(define*-public (keywords-of-type pred #:optional (prefix-str ""))
  "List all symbols bound in the current module that resolve to objects
satisfying pred. Optionally list only symbols starting with prefix-str."
  (filter (lambda (binding)
            (pred (module-ref (current-module) binding)))
          (apropos-internal prefix-str)))

;; Change this name -- its argument is the value not the symbol
(define-public (ly:music-word? obj)
  "Return #t for objects valid when invoked within Lilypond syntax."
  (or (ly:music? obj)
      (ly:music-function? obj)
      (ly:context-mod? obj)
      (ly:translator? obj)
      (ly:score? obj)
      (ly:book? obj)
      (markup? obj)
      ;; These are handled elsewhere since markup functions need their names
      ;; stripped of the -markup(-list) suffix
      ;; (markup-function? obj)
      ;; (markup-list-function? obj)
      (and (markup-list? obj)
           (pair? obj))))

(define-public (ly:event-function? obj)
  (and-let* (((ly:music-function? obj))
             (sig (ly:music-function-signature obj))
             ((pair? (car sig)))
             ((eq? ly:event? (caar sig))))))

(define-public (ly:all-context-names)
  (cons* 'Bottom 'Timing
         (map car (ly:output-description (ly:parser-lookup '$defaultlayout)))))

(define-public (ly:all-translator-names)
  (map ly:translator-name (ly:get-all-translators)))

(define-public (ly:all-grob-names)
  (map car all-grob-descriptions))

(define-public (ly:list-builtin-constants)
  (filter identity
          (module-map (lambda (k v)
                        (and (number? (variable-ref v))
                             k))
                      (resolve-module '(lily)))))

(define*-public (ly:grob-property-completions grob-name
                                              #:optional (subprop '()))
  (let* ((grob-name (if (string? grob-name)
                        (string->symbol grob-name)
                        grob-name))
         (interfaces (assq-ref (assq-ref (assq-ref all-grob-descriptions
                                                   grob-name)
                                         'meta)
                               'interfaces))
         (all-props (append-map (lambda (interface)
                                  (caddr (hashq-ref (ly:all-grob-interfaces)
                                                    interface)))
                                interfaces)))
    (if (null? subprop)
        all-props
        (filter (lambda (p)
                  (eq? symbol-key-alist?
                       (object-property p 'backend-type?)))
                all-props))))

(define-public (ly:accepts-maybe-property-path? f)
  "Return #t for music-functions that accept an argument of type that might be
a property expression e.g. Context.ctxProp or [Context.]Grob.grobProp."
  (and (ly:music-function? f)
       (pair? (lset-intersection eq?
                                 (list symbol-list?
                                       symbol-list-or-music?
                                       symbol-list-or-symbol?
                                       key-list-or-music?
                                       key-list-or-symbol?
                                       key-list?)
                                 (cdr (ly:music-function-signature f))))))
