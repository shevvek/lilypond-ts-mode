;;; navigation.scm --- Moment-location mapping for LilyPond

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
;; along with lilypond-ts-mode.  If not, see <https://www.gnu.org/licenses/>.

(use-modules (ice-9 and-let-star)
             (srfi srfi-26))

((@@ (lily) translator-property-description)
 'navigationTable list? "Collect a list of the first input location for every
moment in each Bottom context.")
((@@ (lily) translator-property-description)
 'navigationExportProps symbol-list? "Context properties to include in exported
navigation data.")

(define-public (selective-map-contexts proc ctx)
  "Apply @var{proc} to context @var{ctx}.  If @var{proc} returns @code{#f},
recurse over the children of @var{ctx}. Returns the tree of contexts where
@var{proc} was successfully applied."
  (or (proc ctx)
      (map (cut selective-map-contexts proc <>)
           (ly:context-children ctx))))

(define (has-timing? ctx)
  "Does context @var{ctx} have a @code{Timing_translator}?"
  ;; A property written exclusively by Timing_translator is a convenient proxy
  ;; for the translator itself. There is no Scheme API to directly query the
  ;; context's translator-group. Faster than output-def->context-def lookup?
  (eq? ctx (ly:context-property-where-defined ctx 'currentBarNumber)))

(define ((record-origins-listener ctx) event)
  (let ((location (ly:event-property event 'origin #f)))
    (when location
      (let* ((fl-ln-ch-cl (ly:input-file-line-char-column location))
             (now (ly:context-current-moment ctx))
             (export-props (ly:context-property ctx 'navigationExportProps
                                                '(measurePosition
                                                  currentBarNumber)))
             (extra-data (filter-map (cut ly:context-property ctx <> #f)
                                     export-props))
             (ev-length (ly:event-property event 'length ZERO-MOMENT))
             (end (if (= 0 (ly:moment-grace now))
                      (+ now ev-length)
                      (- now (ly:make-moment 0 (- (ly:moment-main ev-length))))))
             (score-ctx (ly:context-find ctx 'Score))
             (nav-table (ly:context-property score-ctx 'navigationTable)))
        (ly:context-set-property! score-ctx 'navigationTable
                                  (assoc-set! nav-table (car fl-ln-ch-cl)
                                              (cons (cons* fl-ln-ch-cl
                                                           now end extra-data)
                                                    (assoc-get (car fl-ln-ch-cl)
                                                               nav-table '()))))))))

(define (record-origins! ctx)
  (and (has-timing? ctx)
       (ly:add-listener (record-origins-listener ctx)
                        (ly:context-events-below ctx)
                        'rhythmic-event)))

;; Modified from lily-library.scm
(module-set! (resolve-module '(lily)) 'uniq-list
             (lambda* (lst #:key (key identity) (test equal?) (reverse? #f))
               "Remove adjacent duplicates from @var{lst}. For sorted lists,
faster than @code{delete-duplicates}. Comparisons are performed as
@code{(test (key a) (key b))}, similar to the function of @var{key} and @{test}
keyword arguments in Common LISP. With @var{reverse?} non-@code{#f}, return the
result in reverse order (slightly more efficient)."
               (let ((result (fold (lambda (x acc)
                                     (if (null? acc)
                                         (list x)
                                         (if (test (key x) (key (car acc)))
                                             acc
                                             (cons x acc))))
                                   '() lst)))
                 (if reverse?
                     result
                     (reverse! result '())))))

(define-public ((compare-places test . selectors) a b)
  (and=> (find (lambda (s)
                 (not (equal? (s a) (s b))))
               selectors)
         (lambda (s)
           (test (s a) (s b)))))

(define (sort-by-location file-table)
  (cons (car file-table)
        (uniq-list (sort-list (cdr file-table) (compare-places > cadar caddar))
                   #:key cdar #:reverse? #t)))

(define* (split-sequential-segments lst #:key (key identity) (test <))
  (fold (lambda (x acc)
          (if (and (pair? acc)
                   (test (key (caar acc))
                         (key x)))
              (cons (cons x (car acc))
                    (cdr acc))
              (cons (list x) acc)))
        '() lst))

(define (distribute-index! i seg)
  (map (lambda (elt)
         (set-cdr! elt (cons* i (map (match-lambda ((? ly:moment? m)
                                                    (moment->nav-key m))
                                                   (x x))
                                     (cdr elt))))
         (match elt ((loc j mom end . data)
                     `((,mom . ,end) ,loc ,data))))
       seg))

(define (moment->nav-key m)
  (exact->inexact (ly:moment-main m)))

(define (merge-collated-nav-tables alist1 alist2)
  (if (null? alist1)
      alist2
      (fold (lambda (ftable accum)
              (assoc-set! accum (car ftable)
                          (merge (assoc-get (car ftable) accum '())
                                 (cdr ftable)
                                 (compare-places < caar cadar))))
            alist1 alist2)))

(define-public book-score-nav-tables (make-parameter (list)))
(define-public book-collated-nav-tables (make-parameter (list)))

(define-public (Record_locations_translator ctx)
  (make-translator
   ((initialize translator)
    (selective-map-contexts record-origins! ctx))

   ((finalize translator)
    (ly:progress "\nCollating navigation data...")
    (let* ((nav-table (ly:context-property ctx 'navigationTable))
           (by-file-sorted (map sort-by-location nav-table))
           (seq-segs (append-map (compose reverse
                                          (cut split-sequential-segments <>
                                               #:key cadr)
                                          cdr)
                                 by-file-sorted))
           (by-moment (index-map distribute-index! seq-segs))
           (score-id (string->symbol (format #f "~a-~d-~d"
                                             current-outfile-name
                                             (hash input-file-name 999)
                                             (length (book-score-nav-tables)))))
           (by-file-indexed (map (lambda (ftable)
                                   (cons (car ftable)
                                         (map (match-lambda
                                                (((fname . floc) . data)
                                                 (cons* floc score-id data)))
                                              (cdr ftable))))
                                 by-file-sorted)))
      (book-score-nav-tables (acons score-id by-moment (book-score-nav-tables)))
      (book-collated-nav-tables (merge-collated-nav-tables (book-collated-nav-tables)
                                                           by-file-indexed))))))

(define-public (write-file-to-subdir data name dir ext)
  (let* ((parent-dir (dirname (or input-file-name name)))
         (subdir (string-append parent-dir "/" dir))
         (tmp (begin (mkdir-if-not-exist subdir)
                     (make-tmpfile subdir)))
         (fname (string-append subdir "/" (basename name) ext)))
    (write data tmp)
    (ly:progress "\nConverting to `~a'..." fname)
    (close-port-rename tmp fname)))

(define (default-toplevel-book-handler book)
  (parameterize ((book-score-nav-tables (list))
                 (book-collated-nav-tables (list)))
    (toplevel-book-handler book)
    (write-file-to-subdir `((by-score . (book-score-nav-tables))
                            (by-input-file . (book-collated-nav-tables)))
                          current-outfile-name ".nav" ".l")))
