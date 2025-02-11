(use-modules (ice-9 and-let-star)
             (srfi srfi-26)
             (srfi srfi-71))

(define (moment->nav-key m)
  (exact->inexact (ly:moment-main m)))

;; Maybe record end moment per Voice
(define*-public (record-origins-by-moment #:rest musics)
  (let ((origin-alists '()))
    (define (Record_locations_translator ctx)
      (let ((origin-alist '())
            (got-event-this-step #f))
        (make-translator
         (listeners
          ((rhythmic-event translator event)
           (and-let* (((not got-event-this-step))
                      (now (moment->nav-key (ly:context-current-moment ctx)))
                      (location (ly:event-property event 'origin #f))
                      (loc-info (ly:input-file-line-char-column location)))
             (set! origin-alist
                   (acons now (take loc-info 3) origin-alist))
             (set! got-event-this-step #t))))
         ((stop-translation-timestep translator)
          (set! got-event-this-step #f))
         ((finalize translator)
          (let ((now (moment->nav-key (ly:context-current-moment ctx))))
            (set! origin-alists (cons (cons `(,now) origin-alist)
                                      origin-alists)))))))

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
  (let* ((sort-voices (sort-list origin-alists
                                 (lambda (a b)
                                   ;; Sort by first line of expression
                                   ;; But only within the same file
                                   ;; Leave file order unchanged
                                   (let ((a-beg-loc (cdr (last a)))
                                         (b-beg-loc (cdr (last b))))
                                     (and (equal? (car a-beg-loc)
                                                  (car b-beg-loc))
                                          (< (cadr a-beg-loc)
                                             (cadr b-beg-loc)))))))
         (voice-indices (index-map (lambda (i origin-alist)
                                     (map (lambda (elt)
                                            `(,@(cdr elt) ,(car elt) ,i))
                                          ;; Head is just a moment marking
                                          ;; the end bound of expression
                                          (cdr origin-alist)))
                                   sort-voices))
         (sort-by-ch (sort-list (concatenate voice-indices)
                                (comparator-from-key caddr <)))
         (sort-by-ln (sort-list sort-by-ch (comparator-from-key cadr <)))
         (split-by-file (split-by-car sort-by-ln)))
    `((by-file . ,split-by-file)
      (by-moment . ,sort-voices))))
