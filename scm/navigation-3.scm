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
             (srfi srfi-26)
             (srfi srfi-71))

((@@ (lily) translator-property-description)
 'navigationTable list? "Collect a list of the first input location for every
moment in each Bottom context.")

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
  (and-let* (((location (ly:event-property event 'origin #f)))
             (fl-ln-ch-cl (ly:input-file-line-char-column location))
             (now (ly:context-current-moment ctx))
             (barn (ly:context-property ctx 'currentBarNumber))
             (mpos (ly:context-property ctx 'measurePosition))
             (ev-length (ly:event-property event 'length ZERO-MOMENT))
             (end (if (= 0 (ly:moment-grace now))
                      (+ now ev-length)
                      (- now (ly:make-moment 0 (- (ly:moment-main ev-length))))))
             (score-ctx (ly:context-find ctx 'Score))
             (nav-table (ly:context-property score-ctx 'navigationTable)))
    (ly:context-set-property! score-ctx 'navigationTable
                              (assoc-set! nav-table (car fl-ln-ch-cl)
                                          (cons (list fl-ln-ch-cl
                                                      now end barn mpos)
                                                (assoc-get (car fl-ln-ch-cl)
                                                           nav-table '()))))))

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
  (uniq-list (sort-list file-table (compare-places > cadar caddar))
             #:key cdar #:reverse? #t))

(define* (split-sequential-segments lst #:key (key identity) (test <))
  (fold (lambda (x acc)
          ;; This awkward let block is because Guile lacks prog1/begin0
          (let ((res (if (and (pair? acc)
                              (test (key (cdaar acc))
                                    (key x)))
                         (cons (cons x (car acc))
                               (cdr acc))
                         (cons (list x) acc))))
            ;; The purpose of doing this is to smuggle the segment indices into
            ;; the file-collated list without having to iterate over the entire
            ;; list to reverse-lookup every location
            (set-cdr! x (cons (1- (length res)) (cdr x)))
            res))
        '() lst))

(define-public book-nav-tables (make-parameter (list)))

(define-public (Record_locations_translator ctx)
  (make-translator
   ((initialize translator)
    (selective-map-contexts has-timing? record-origins! ctx))
   ((finalize translator)
    (let* ((nav-table (ly:context-property ctx 'navigationTable))
           (by-file-sorted (map sort-by-location nav-table))
           (seq-segs (append-map (compose reverse
                                          (cut split-sequential-segments <>
                                               #:key cadr)
                                          cdr)
                                 by-file-sorted)))
      (book-nav-tables `(((by-file . ,by-file-sorted)
                          (by-moment . ,seq-segs))
                         ,(book-nav-tables)))))))

(define (moment->nav-key m)
  (exact->inexact (ly:moment-main m)))

(define (merge-nav-file-alists by-file-alists)
  (reduce (lambda (alist1 alist2)
            (fold (lambda (ftable accum)
                    (assoc-set! accum (car ftable)
                                (merge (assoc-get (car ftable) accum '())
                                       (cdr ftable)
                                       (compare-places < caar cadar))))
                  alist1 alist2))
          '() by-file-alists))

(define (make-score-id index)
  (string->symbol
   (format #f "~a-~d-~d" current-outfile-name (hash input-file-name 999) index)))

(define-public (map-squared proc lst)
  (map (cut map proc <>) lst))

(define (distribute-indices score-index score-table)
  (let* ((score-id (make-score-id score-index))
         (by-file (map-squared (match-lambda
                                 (((fname . floc) j . rhythmic-data)
                                  (list-set! rhythmic-data 0
                                             (moment->nav-key (car rhythmic-data)))
                                  (list-set! rhythmic-data 1
                                             (moment->nav-key (cadr rhythmic-data)))
                                  `(,floc (,score-id ,j) ,rhythmic-data)))
                               (assq-ref score-table 'by-file)))
         (by-moment (map-squared (match-lambda
                                   ((loc j mom end . other-rhythmic-data)
                                    `((,mom ,end) ,loc ,other-rhythmic-data)))
                                 (assq-ref score-table 'by-moment))))
    (cons by-file (cons score-id by-moment))))

(define-public (write-file-to-subdir data name dir ext)
  (let* ((parent-dir (dirname (or input-file-name name)))
         (subdir (string-append parent-dir "/" dir))
         (tmp (begin (mkdir-if-not-exist subdir)
                     (make-tmpfile subdir)))
         (fname (string-append subdir "/" (basename name) ext)))
    (write data tmp)
    (ly:progress "\nConverting to `~a'..." fname)
    (close-port-rename tmp fname)))

(define-public (make-book-nav-table)
  (let* ((format-elts (index-map distribute-indices (book-nav-tables)))
         (by-score (map cdr format-elts))
         (by-file (merge-nav-file-alists (map car format-elts))))
    (ly:progress "\nExporting navigation data for book `~a'..."
                 current-outfile-name)
    (write-file-to-subdir `((by-score . ,by-score)
                            (by-input-file . ,by-file))
                          book-name ".nav" ".l")))

(define (default-toplevel-book-handler book)
  (parameterize ((book-nav-tables (list)))
    (toplevel-book-handler book)
    (make-book-nav-table)))
