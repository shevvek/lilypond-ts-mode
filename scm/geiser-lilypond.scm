;;; geiser-lilypond.scm --- Lilypond layer for Geiser Guile

;; Copyright (c) 2025 Saul James Tobin

;; This file is part of lilyond-ts-mode.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with lilyond-ts-mode.  If not, see <https://www.gnu.org/licenses/>.

(define-module (geiser-lilypond)
  #:use-module (ice-9 and-let-star)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 session)
  #:use-module (ice-9 documentation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (system vm program)
  #:use-module (lily)
  #:use-module (lily curried-definitions))

(define-private gutils (resolve-module '(geiser utils)))
(define-private gdoc (resolve-module '(geiser doc)))

(define-public (safe-load filename)
  (let ((path (string-append "lily/" filename)))
    (when (%search-load-path path)
      (primitive-load-path path))))

(safe-load "document-identifiers")
(safe-load "define-paper-variables")

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
    (let*-values (((req-args rest) (split-at arg-names req-count))
                  ((opt-args rest) (split-at rest opt-count))
                  ((kw-args rest) (split-at rest kw-count))
                  ((rest) (and (pair? rest) (car rest))))
      `(((required . ,req-args)
         (optional . ,opt-args)
         (keyword . ,kw-args)
         (rest . ,rest))))))

(define (ly:zip-function-arguments arg-names arg-types return-type)
  (and-let* (((= (length arg-names)
                 (length arg-types)))
             (arg-spec (map (lambda (name type)
                              (if (pair? type)
                                  `(,(procedure-name (car type))
                                    ,name
                                    ,(cdr type))
                                  `(,(procedure-name type)
                                    ,name)))
                            arg-names arg-types)))
    `((type . ,return-type)
      (arguments . ,arg-spec))))

(define (ly:music-function-arguments mf)
  (and-let* (((ly:music-function? mf))
             (sig (ly:music-function-signature mf))
             (func (ly:music-function-extract mf))
             (arg-names (syntax-function-procedure-arguments func))
             (return-type (procedure-name (if (pair? (car sig))
                                              (caar sig)
                                              (car sig)))))
    (ly:zip-function-arguments arg-names (cdr sig) return-type)))

(define (ly:markup-function-arguments mf)
  (and-let* ((return-type (cond ((markup-function? mf) 'markup?)
                                ((markup-list-function? mf) 'markup-list?)
                                (else #f)))
             (arg-names (drop (assq-ref (program-arguments-alist mf) 'required)
                              2))
             (arg-types (markup-command-signature mf)))
    (ly:zip-function-arguments arg-names arg-types return-type)))

(let ((old-args (module-ref gdoc 'arguments))
      (old-sig (module-ref gdoc 'signature))
      (old-docstring (module-ref gdoc 'docstring))
      (maybe-texinfo (module-ref gdoc 'try-texinfo->plain-text)))
  (module-set! gdoc 'arguments
               (lambda (proc)
                 (or (ly:music-function-arguments proc)
                     (ly:markup-function-arguments proc)
                     (old-args proc)
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

(let ((old-sym->obj (module-ref gutils 'symbol->object)))
  (module-set! gutils 'symbol->object
               (lambda (sym)
                 (or (old-sym->obj sym)
                     (old-sym->obj (symbol-append sym '-markup))
                     (old-sym->obj (symbol-append sym '-markup-list))))))

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
