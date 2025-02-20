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

(define* ((split-by-key selector #:optional (segment-proc identity))
          l #:optional (accum-result '()))
  (let* ((k (selector (car l)))
         (same-key other-keys (partition (lambda (el)
                                           (equal? (selector el) k))
                                         l))
         (accum-result (cons (segment-proc same-key) accum-result)))
    (if (pair? other-keys)
        ((split-by-key selector segment-proc) other-keys accum-result)
        accum-result)))

(define (finalize-score-nav-table nt)
  (append-map uniq-list
              ((split-by-key (compose cadr last))
               (sort-list (reverse nt)
                          (comparator-from-key (compose caddr last)
                                               <)))))

(define*-public (bottom-context-music? m #:optional odef)
  (and-let* (((ly:music? m))
             ((music-is-of-type? m 'context-specification))
             (odef (if (ly:output-def? odef) odef
                       (ly:parser-lookup '$defaultlayout)))
             (ctx-type (ly:music-property m 'context-type))
             (bottom-names (map car (ly:output-find-context-def odef 'Bottom)))
             ((or (eq? ctx-type 'Bottom)
                  (memq ctx-type bottom-names))))))

(define*-public (voicify-all-simultaneous m #:optional odef)
  (music-map (lambda (s)
               (if (music-is-of-type? s 'simultaneous-music)
                   (map-some-music (lambda (elt)
                                     (cond
                                      ((music-is-of-type? elt 'sequential-music)
                                       (context-spec-music elt 'Bottom
                                                           (symbol->string
                                                            (gensym))))
                                      ((bottom-context-music? elt odef)
                                       elt)
                                      (else #f)))
                                   s)
                   s))
             m))

(define-public toplevel-nav-functions
  (list
   (ly:music-function-extract (ly:parser-lookup 'killCues))
   (lambda (music)
     (music-filter (negate (music-type-predicate 'translator-change-instruction))
                   music))
   ;; must be last so that when the list is composed, optional odef arg accepted
   voicify-all-simultaneous))

(define-public (record-nav-data music odef)
  (and-let* ((nav-music ((apply compose toplevel-nav-functions)
                         (ly:music-deep-copy music)
                         odef))
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

(define (score->nav-table score)
  (and-let* ((music (ly:score-music score))
             (odef (find (lambda (o)
                           (eq? 'navigation
                                (ly:output-def-lookup o 'output-def-kind)))
                         (ly:score-output-defs score))))
    (record-nav-data music odef)))

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
    ((split-by-key car (lambda (seg)
                         (cons (caar seg) (map cdr seg)))) sort-by-ln)))

(define (collate-nav-data nav-tables)
  (ly:progress "\nCollating navigation data...")
  (let ((with-indices (map (lambda (score-table)
                             (index-map (distribute-indices (car score-table))
                                        (cdr score-table)))
                           nav-tables)))
    (sort-by-location (append-map concatenate with-indices))))

(define-public (write-file-to-subdir data name dir ext)
  (let* ((subdir (string-append (dirname name) "/" dir))
         (tmp (begin (mkdir-if-not-exist subdir)
                     (make-tmpfile subdir)))
         (fname (string-append subdir "/" (basename name) ext)))
    (write data tmp)
    (ly:progress "\nConverting to `~a'..." fname)
    (close-port-rename tmp fname)))

(define (book-scores-flatten book)
  `(,@(ly:book-scores book)
    ,@(append-map book-scores-flatten (ly:book-book-parts book))))

(define-public (make-book-nav-table book)
  (and-let* ((book-name ((@@ (lily) get-outfile-name) book))
             (all-scores (filter ly:score? (book-scores-flatten book)))
             ((pair? all-scores))
             (score-tables (filter-map score->nav-table all-scores))
             ((pair? score-tables))
             (score-alist (index-map (lambda (j score-table)
                                       (cons (string->symbol
                                              (format #f "~a-~a"
                                                      book-name j))
                                             score-table))
                                     score-tables))
             (collate-by-input-file (collate-nav-data score-alist)))
    (ly:progress "\nExporting navigation data for book `~a'..." book-name)
    (write-file-to-subdir `((by-score . ,score-alist)
                            (by-input-file . ,collate-by-input-file))
                          book-name ".nav" ".l")))

;; Calling print-book-with-defaults first, then make-book-nav-table using
;; current-outfile-name, would give equivalent output and avoid minor code
;; duplication, but really navigation should run first since it should be
;; fast compared to typesetting.
(define default-toplevel-book-handler
  (lambda (book)
    (let ((paper (ly:parser-lookup '$defaultpaper))
          (layout (ly:parser-lookup '$defaultlayout)))
      (make-book-nav-table book)
      (ly:book-process book paper layout current-outfile-name))))

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
