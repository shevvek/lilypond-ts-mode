(use-modules (ice-9 and-let-star)
             (srfi srfi-26)
             (srfi srfi-71))

(define*-public (record-origins-by-moment #:rest musics)
  (let ((origin-alists '()))
    (define (Record_locations_translator ctx)
      (let ((origin-alist '())
            (got-event-this-step #f))
        (make-translator
         (listeners
          ((music-event translator event)
           (and-let* (((not got-event-this-step))
                      (now (ly:moment-main (ly:context-current-moment ctx)))
                      (location (ly:event-property event 'origin #f))
                      (loc-info (ly:input-file-line-char-column location)))
             (set! origin-alist
                   (acons (exact->inexact now) (take loc-info 3) origin-alist))
             (set! got-event-this-step #t))))
         ((stop-translation-timestep translator)
          (set! got-event-this-step #f))
         ((finalize translator)
          (set! origin-alists (cons origin-alist origin-alists))))))

    (ly:run-translator (make-simultaneous-music
                        (map (lambda (m)
                               #{ \new Staff { #m } #})
                             musics))
                       #{
                         \layout {
                           \partCombineListener
                           \context {
                             \Voice
                             \consists #Record_locations_translator
                           }
                         }
                       #})
    (reverse origin-alists)))

(define* (split-by-car l #:optional (accum-result '()))
  (let* ((k (caar l))
         (same-key other-keys (partition (lambda (el)
                                           (equal? (car el) k))
                                         l))
         (alist-entry (cons k (map cdr same-key)))
         (accum-result (cons alist-entry accum-result)))
    (if (pair? other-keys)
        (split-by-car other-keys accum-result)
        accum-result)))

(define-public (sort-moment-origin-table origin-alists)
  (let* ((voice-indices (index-map (lambda (i origin-alist)
                                     (map (lambda (elt)
                                            `(,@(cdr elt) ,(car elt) ,i))
                                          origin-alist))
                                   origin-alists))
         (sort-by-ch (sort-list (concatenate voice-indices)
                                (comparator-from-key caddr <)))
         (sort-by-ln (sort-list sort-by-ch (comparator-from-key cadr <)))
         (split-by-file (split-by-car sort-by-ln)))
    `((by-file . ,split-by-file)
      (by-moment . ,origin-alists))))
