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
 'navigationTable list? "Collect a list of the first input location for every
moment in each Bottom context.")

(define-public (Record_locations_translator ctx)
  (let ((origin-alist '())
        (got-event-this-step #f)
        (busy-until #f))
    (make-translator
     (listeners
      ((rhythmic-event translator event)
       (and-let* ((location (ly:event-property event 'origin #f))
                  (now (ly:context-current-moment ctx))
                  (ev-end (ly:event-property event 'length))
                  (new-until (+ now ev-end)))
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
                 (score (ly:context-find ctx 'Score))
                 (accum (ly:context-property score 'navigationTable)))
        (ly:context-set-property! score 'navigationTable
                                  (cons (cons `(,(moment->nav-key now))
                                              (list-copy origin-alist))
                                        accum))
        (set! origin-alist '())
        (set! busy-until #f))
      (set! got-event-this-step #f)))))

(ly:register-translator
 Record_locations_translator 'Record_locations_translator
 '((grobs-created . ())
   (events-accepted . (rhythmic-event))
   (properties-read . (navigationTable))
   (properties-written . (navigationTable))
   (description . "\
Collect the moment (as a decimal) and input location (as filename, line, char)
of context's first rhythmic-event in each timestep, plus the moment the context
ends (without location). Start a new list if there is a gap in which context has
no active rhythmic-events.")))

(define ((purify-translators allowed) ctx-def)
  (let* ((consists (ly:context-def-lookup ctx-def 'consists))
         (purge-list (lset-difference eq? consists allowed))
         (mod (ly:make-context-mod (map (lambda (t)
                                          `(remove ,t)) purge-list))))
    (ly:context-def-modify ctx-def mod)))

(define (make-minimal-output-def odef)
  (let ((new-odef (ly:output-def-clone odef))
        (purify-proc (purify-translators '(Timing_translator
                                           Mark_tracking_translator
                                           Record_locations_translator))))
    (for-each (lambda (ctx-def-pair)
                (ly:output-def-set-variable! new-odef (car ctx-def-pair)
                                             (purify-proc (cdr ctx-def-pair))))
              (ly:output-find-context-def new-odef))
    new-odef))

(define-public navigation
  (define-scheme-function (odef)
    ((ly:output-def? (ly:parser-lookup '$defaultmidi)))
    (let* ((minimal-clone (make-minimal-output-def odef))
           (voice-def (ly:output-def-lookup minimal-clone 'Voice))
           (nav-ctx-mod (ly:make-context-mod
                         '((consists Record_locations_translator)))))
      (ly:output-def-set-variable! minimal-clone
                                   'Voice
                                   (ly:context-def-modify voice-def
                                                          nav-ctx-mod))
      (ly:output-def-set-variable! minimal-clone
                                   'output-def-kind
                                   'navigation)
      ;; At present, translator groups must either be performer or engraver
      ;; type, and likewise for scores.
      (ly:expect-warning (G_ "cannot create a zero-track MIDI file"))
      minimal-clone)))

(define (finalize-score-nav-table nt)
  (uniq-list (sort-list (reverse nt) (lambda (a b)
                                       ;; Sort by first line of expression
                                       ;; But only within the same file
                                       ;; Leave file order unchanged
                                       (let ((a-beg-loc (cdr (last a)))
                                             (b-beg-loc (cdr (last b))))
                                         (and (equal? (car a-beg-loc)
                                                      (car b-beg-loc))
                                              (< (cadr a-beg-loc)
                                                 (cadr b-beg-loc))))))))

(define-public toplevel-nav-functions
  (list
   (ly:music-function-extract (ly:parser-lookup 'killCues))
   (lambda (music)
     (music-filter (lambda (m)
                     (not (music-is-of-type? m 'translator-change-instruction)))
                   music))))

(define-public (record-nav-data music odef)
  (and-let* ((nav-music ((apply compose toplevel-nav-functions)
                         (ly:music-deep-copy music)))
             (translation-done (ly:run-translator nav-music odef))
             ;; Retrieving nav data from Score context property via
             ;; Global ctx post-translation, then run post-processing
             ;; Before output to a file is directly parallel to the
             ;; way midi and print output works, but hooking into
             ;; that system requires adding a new subclass of Music_ouput
             (score-ctxs (ly:context-children translation-done))
             ((pair? score-ctxs))
             (nav-table (ly:context-property (car score-ctxs)
                                             'navigationTable)))
    (ly:progress "\nExtracting navigation data...")
    (finalize-score-nav-table nav-table)))

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

(define ((distribute-indices . other-indices) voice-index lst)
  (map (lambda (elt)
         `(,@(cdr elt) ,(car elt) ,voice-index ,@other-indices))
       ;; car of a nav segment is just a moment marking the end of a music
       ;; expression, with no location.
       (cdr lst)))

(define (sort-by-location combined-nav-data)
  (let* ((sort-by-ch (sort-list combined-nav-data
                                (comparator-from-key caddr <)))
         (sort-by-ln (sort-list sort-by-ch
                                (comparator-from-key cadr <))))
    (split-by-car sort-by-ln)))


;;; Old Emacs lilypond-ts-mode moment nav API

(define-public (record-origins-by-moment . musics)
  (let ((music ((apply compose toplevel-music-functions)
                (make-simultaneous-music (map (lambda (m)
                                                #{ \new Staff { #m } #})
                                              musics)))))
    (record-nav-data music (navigation))))

(define-public (sort-moment-origin-table nav-table)
  (let* ((voice-indices (index-map (distribute-indices) nav-table))
         (split-by-file (sort-by-location (concatenate voice-indices))))
    `((by-file . ,split-by-file)
      (by-moment . ,nav-table))))
