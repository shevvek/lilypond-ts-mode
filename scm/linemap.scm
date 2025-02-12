;;; linemap.scm --- Moment-location mapping for LilyPond

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

(define (moment->nav-key m)
  (exact->inexact (ly:moment-main m)))

((@@ (lily) translator-property-description)
 'recordLocationsCallback procedure? "Procedure called by
@code{Record_locations_translator} during finalize to collect and export
moment-location data from this context. Should accept a single argument:
an alist mapping rhythmic positions to input locations.")

(define-public (Record_locations_translator ctx)
  (let ((origin-alist '())
        (got-event-this-step #f)
        (busy-until #f))
    (make-translator
     (listeners
      ((rhythmic-event translator event)
       (and-let* ((location (ly:event-property event 'origin #f))
                  (now (ly:context-current-moment ctx))
                  (ev-end (ly:event-property event 'duration ZERO-DURATION))
                  (new-until (+ now (ly:duration->moment ev-end))))
         (unless (and busy-until
                      (ly:moment<? new-until busy-until))
           (set! busy-until new-until))
         (unless got-event-this-step
           (set! origin-alist
                 (acons (moment->nav-key now)
                        (take (ly:input-file-line-char-column location) 3)
                        origin-alist))
           (set! got-event-this-step #t)))))
     ((stop-translation-timestep translator)
      (and-let* (((pair? origin-alist))
                 (busy-until)
                 (now (ly:context-current-moment ctx))
                 ((or (equal? busy-until now)
                      (ly:moment<? busy-until now)))
                 (callback (ly:context-property ctx 'recordLocationsCallback
                                                (const #f))))
        (callback (cons `(,(moment->nav-key now)) (list-copy origin-alist)))
        (set! origin-alist '())
        (set! busy-until #f))
      (set! got-event-this-step #f)))))

(ly:register-translator
 Record_locations_translator 'Record_locations_translator
 '((grobs-created . ())
   (events-accepted . (rhythmic-event))
   (properties-read . (recordLocationsCallback))
   (properties-written . ())
   (description . "\
Collect the moment (as a decimal) and input location (as filename, line, char)
of context's first rhythmic-event in each timestep, plus the moment the context
ends (without location). Export the resulting list by calling
@code{recordLocationsCallback}.")))

;; Maybe record end moment per Voice
(define*-public (record-origins-by-moment #:rest musics)
  (let ((origin-alists '()))
    (ly:run-translator (make-simultaneous-music
                        (map (lambda (m)
                               #{ \killCues \new Staff { #m } #})
                             musics))
                       #{
                         \layout {
                           \partCombineListener
                           \context {
                             \Voice
                             \consists Record_locations_translator
                             recordLocationsCallback =
                               #(lambda (l)
                                 (set! origin-alists (cons l origin-alists)))
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
