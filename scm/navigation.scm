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

(use-modules (srfi srfi-26))

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

(define*-public ((lexicographic-comparator-from-keys test #:key (= equal?)
                                                     . selectors) . args)
  "Make a general lexicographic comparator that returns the result of @var{test}
for the first non-@code{equal?} set of values selected by applying each of
@var{selectors} in turn across the comparator's arguments. With
@code{#:=}@tie{}@var{=}, use the provided @var{=} proc instead of @code{equal?}.

For example, @code{(lexicographic-comparator-from-keys < car cadr caddr #:= =)}
would yield a function appropriate for comparing LilyPond version numbers in
list-form (e.g. @code{(2 25 20)})."
  (and=> (any (lambda (s) ; relies on `any' checking elements in order
                (let ((vals (map s args)))
                  (and (not (apply = vals))
                       vals)))
              selectors)
         (lambda (vals)
           (apply test vals))))

(define-public (write-file-to-subdir data dir name ext)
  "Write @var{data} to @var{dir}/@var{name}.@var{ext} relative to the current
input file."
  (let* ((parent-dir (dirname (or input-file-name name)))
         (subdir (string-append parent-dir "/" dir))
         (tmp (begin (mkdir-if-not-exist subdir)
                     (make-tmpfile subdir)))
         (fname (string-append subdir "/" (basename name) ext)))
    (write data tmp)
    (ly:progress "\nConverting to `~a'..." fname)
    (close-port-rename tmp fname)))

(define-public (selective-map-contexts proc ctx)
  "Apply @var{proc} to context @var{ctx}.  If @var{proc} returns @code{#f},
recurse over the children of @var{ctx}. Returns the tree of contexts where
@var{proc} was successfully applied."
  (or (proc ctx)
      (map (cut selective-map-contexts proc <>)
           (ly:context-children ctx))))

(define*-public (split-sequential-segments lst #:key (key identity) (test <))
  "Split @var{lst} into segments such that @code{(@var{key} elt)} varies
monotonically under @var{test} within each segment. This reverses the initial
ordering of @var{lst}."
  (fold (lambda (x acc)
          (if (and (pair? acc)
                   (test (key (caar acc))
                         (key x)))
              (cons (cons x (car acc))
                    (cdr acc))
              (cons (list x) acc)))
        '() lst))

(define*-public (merge-sorted-tables alist1 alist2
                                     #:optional (comparator <))
  "Combine two alists @var{alist1} and @var{alist2}, where the values are lists
sorted under @var{comparator}. Merge the value lists for any keys common to both
alists. Order of keys is not preserved."
  (if (null? alist1)
      alist2
      (fold (lambda (ftable accum)
              (assoc-set! accum (car ftable)
                          (merge (assoc-get (car ftable) accum '())
                                 (cdr ftable)
                                 comparator)))
            alist1 alist2)))

((@@ (lily) translator-property-description)
 'navigationTable list? "Mapping between input locations and rhythmic positions
for every @code{rhythmic-event} in @code{Score} up to the current timestep.")
((@@ (lily) translator-property-description)
 'navigationExportProperties symbol-list? "Context properties to include in
exported navigation data.")

(define-public score-nav-tables (make-parameter (list)))
(define-public collated-nav-tables (make-parameter (list)))
(define-public book-score-count (make-parameter 0))

(define-public (sort-by-location nav-table)
  "If @var{nav-table} is in the format of @code{Score.navigationTable}, sort it
by @var{line} and @var{character}, then remove duplicates by comparing the input
locations of adjacent elements. Filenames are ignored, because navigation tables
are pre-sorted by filename during translation."
  (uniq-list (sort-list nav-table
                        (lexicographic-comparator-from-keys > cadar caddar))
             ;; sort backwards so that uniq-list doesn't need to call reverse!
             #:key cdar #:reverse? #t))

(define-public (moment->nav-key m)
  ;; Most other languages, including Emacs Lisp, do not support exact rationals.
  ;; Discard grace timing so editors don't have to worry about moment math.
  (exact->inexact (ly:moment-main m)))

(define (distribute-index-and-format! i seg)
  (map (lambda (elt)
         ;; The reason for set-cdr! is to smuggle indices and formatted moments
         ;; from the segmented list to the collated list via shared cons tails
         ;; of the elements. Otherwise this would require iterating over the
         ;; whole list to reverse lookup every element. This match-lambda would
         ;; also be the place to add patterns to format custom export properties
         (set-cdr! elt (cons* i (map (match-lambda ((? ly:moment? m)
                                                    (moment->nav-key m))
                                                   (x x))
                                     (cdr elt))))
         (match elt ((loc j mom end . data)
                     `((,mom . ,end) ,loc ,data))))
       seg))

(define-public (export-nav-data)
  "Write the navigation data for the current book, accumulated in
@code{score-nav-tables} and @code{collated-nav-tables}, to
@file{.nav/@var{bookname}.l}, relative to the input file location."
  (ly:progress "\nExporting navigation data for book `~a'..."
               current-outfile-name)
  (write-file-to-subdir `((by-score . ,(score-nav-tables))
                          (by-input-file . ,(collated-nav-tables)))
                        ;; .l because the data is generic Scheme/LISP format
                        ;; Use a fixed subdirectory relative to the input file
                        ;; (not the output directory) so editors can find it.
                        ".nav" current-outfile-name ".l"))

(define-public (finalize-nav-data! nav-table)
  "Process data collected in a @code{Score}'s @code{navigationTable}. Sort,
remove duplicate events, split into sequential music expressions, collate by
file, format for export. The processed data is stored in the book-level
parameters @code{score-nav-tables} and @code{collated-nav-tables}. If data has
been processed for all scores in the book, write both parameters to the output
file, then clear @code{score-nav-tables}."
  (ly:progress "\nCollating navigation data...")
  ;; Initially discard the filename alist keys, as filenames are in each element
  ;; Sort within each filename by line and char, then remove duplicates.
  (let* ((sorted-by-location (map (compose sort-by-location cdr) nav-table))
         ;; Split the sorted list wherever adjacent elements are not sequential
         ;; rhythmically. Doing it this way automatically handles music shared
         ;; between staves, context changes, quotes/cues, and simultaneous music
         ;; expressions within the same Bottom context. It does mean that two
         ;; separate music expressions that happen one after the other will be
         ;; treated as a single expression for navigation purposes.
         (sequential-segs (append-map (compose reverse
                                               (cut split-sequential-segments <>
                                                    #:key cadr))
                                      sorted-by-location))
         ;; Imperatively insert segment indices into the elements and process
         ;; moments into export format, so this data is available for collation
         (by-moment (index-map distribute-index-and-format! sequential-segs))
         (score-index (length (score-nav-tables)))
         ;; score-ids are sufficiently unique for editors to use a single global
         ;; lookup table across all projects
         (score-id (string->symbol (format #f "~a-~d-~d"
                                           current-outfile-name
                                           ;; disambiguate books with same name
                                           (hash input-file-name 999)
                                           score-index)))
         ;; Format the elements of the filename collated version of the data
         ;; Restore the filename alist key that was stripped earlier
         (by-file-with-indices (map (lambda (ftable)
                                      (cons (caaar ftable) ; filename of ftable
                                            (map (match-lambda
                                                   (((fname . floc) . data)
                                                    (cons* floc score-id data)))
                                                 ftable)))
                                    sorted-by-location)))
    (score-nav-tables (acons score-id by-moment (score-nav-tables)))
    (collated-nav-tables (merge-sorted-tables
                          (collated-nav-tables) by-file-with-indices
                          (lexicographic-comparator-from-keys < caar cadar)))
    (when (= score-index (1- (book-score-count)))
      ;; Write to file as soon as translation is done for the last score in the
      ;; book, so editor navigation refresh doesn't have to wait for typesetting
      (export-nav-data)
      ;; If some score in the book didn't generate nav data, this clause never
      ;; runs. We reset the value of one of the parameters here as a flag to
      ;; tell toplevel-book-handler that nav data was successfully exported at
      ;; the end of translation. If score-nav-tables still has data after
      ;; typesetting, toplevel-book-handler knows to run export-nav-data.
      (score-nav-tables #f))))

(define-public ((record-origins-listener ctx) event)
  "Make a listener callback for context @var{ctx} that gathers each event's
input location. start and end moment, and values of each property in
@code{navigationExportProperties}. Collect the data under alist entries per
input filename in @code{Score}@tie{}level property @code{navigationTable}. Each
entry is of the form:
@code{((filename line char col) . (beg-moment end-moment . export-props))}"
  (let ((location (ly:event-property event 'origin #f)))
    (when location ; ignore synthetic events
      (let* ((fl-ln-ch-cl (ly:input-file-line-char-column location))
             (now (ly:context-current-moment ctx))
             (export-props (ly:context-property ctx 'navigationExportProperties))
             (extra-data (filter-map (cut ly:context-property ctx <> #f)
                                     export-props))
             (ev-length (ly:event-property event 'length ZERO-MOMENT))
             ;; rhythmic-events within grace music have non-grace length, so if
             ;; now has a grace component, convert ev-length to a pure grace
             ;; moment first, then calculate end-moment
             (end (if (= 0 (ly:moment-grace now))
                      (+ now ev-length)
                      (- now (ly:make-moment 0 (- (ly:moment-main ev-length))))))
             (score-ctx (ly:context-find ctx 'Score))
             (nav-table (ly:context-property score-ctx 'navigationTable))
             (file-table (assoc-get (car fl-ln-ch-cl) nav-table '())))
        ;; alist assignment is a convenient way to pre-sort events by file,
        ;; preserving score order when cycling through music expressions
        (ly:context-set-property! score-ctx 'navigationTable
                                  (assoc-set! nav-table (car fl-ln-ch-cl)
                                              (cons (cons* fl-ln-ch-cl
                                                           now end extra-data)
                                                    file-table)))))))

(define-public (record-origins-where-timing! ctx)
  "Add @code{record-origins-listener} to @var{ctx}, if @var{ctx} has a
@code{Timing_translator}; otherwise return @code{#f}."
  (and (eq? ctx (ly:context-find ctx 'Timing))
       (ly:add-listener (record-origins-listener ctx)
                        (ly:context-events-below ctx)
                        'rhythmic-event)))

(define-public (Record_locations_translator ctx)
  (make-translator
   ((initialize translator)
    ;; Since we might want to export metric info as part of nav data, we should
    ;; record events in the same context where Timing_translator lives.
    ;; To avoid needing multiple translators and manual user config, instead of
    ;; writing the listener normally as part of the translator def, we search
    ;; recursively for Timing contexts and explicitly add the listener there.
    (selective-map-contexts record-origins-where-timing! ctx))

   ((finalize translator)
    (let ((nav-table (ly:context-property ctx 'navigationTable)))
      (when (pair? nav-table)
        ;; This is an atypical translator because it reads and writes book-level
        ;; state, and, if it is the last score in the book, writes to disk.
        ;; Its behavior (incrementally recording data per-event, applying both
        ;; score and book post-processing, then writing to output) arguably fits
        ;; the template of a Score translator-group/music-output/output-def type
        ;; similar to \midi{} or \layout{}. The principal advantage to this
        ;; implementation is that it can be attached to $defaultlayout and hence
        ;; work with zero-modification to user code, with existing LilyPond
        ;; releases, simply by including this code via -dinclude-settings,
        ;; Enabling it to be deployed by editing software without waiting for
        ;; users to upgrade to a future LilyPond version that may upstream this.
        (finalize-nav-data! nav-table))))))

(ly:register-translator
 Record_locations_translator 'Record_locations_translator
 '((grobs-created . ())
   (events-accepted . (rhythmic-event))
   (properties-read . (navigationTable
                       navigationExportProperties))
   (properties-written . (navigationTable))
   (description . "\
Collect the start moment, end moment, input location, and value of each property
in @code{navigationExportProperties} for every @code{rhythmic-event}. Organize
this data into a two tables: one collated by filename, the other split into
discrete sequential music expressions. Add the organized data to the book-level
parameters @code{collated-nav-tables} and @code{score-nav-tables} respectively.
If this @code{Score} is the last one in its top-level book, write the collected
data from both paramaters to a file named @file{.nav/@code{bookname}.l}, located
relative to the file being compiled.

Each nav file contains a single generic Scheme/LISP-compatible alist sexp. The
format is as follows:

@example
((by-score
  . ((score_id_0 . score_list_0)
     (score_id_1 . score_list_1)
     ...
     (score_id_n . score_list_n)))
 (by-input-file
  . ((filename_0 . file_alist_0)
     (filename_1 . file_alist_1)
     ...
     (filename_n . file_alist_n))))

score_list_i: (segment_0 segment_1 ... segment_n)
seg_j:   (event_n event_n-1 ... event_0)
event_k: ((beg-moment . end-moment) (filename line char col) export-props)

file_alist_i: ((location_0 . data_0)
               (location_1 . data_1)
               ...
               (location_n . data_n))
location_j: (line char col)
data_j: (score-id segment-index beg-moment end-moment . export-props)
@end example

Each alist in @code{by-input-file} is sorted by ascending input location. The
intent is that an editor such as Frescobaldi or Emacs will use this data to
iterate over LilyPond code attaching rhythmic metadata.

The list associated with each @code{score-id} in @code{by-score} is sorted in
the order each sequential music segment appears in the code, but the list of
events within each segment is sorted in reverse order. The intent is that an
editor will keep the @code{by-score} alist in memory as a lookup table.

This setup facilitates the implementation of @qq{vertical} navigation through
the code for a score, cycling between the input locations corresponding to a
given rhythmic position across all polyphonic parts, via the following lookup
chain:
@enumerate

@item @code{score-id = this-score-id}

@item
@code{segment-id = destination-segment-id}, for cycling forward or backward,
@code{this-segment-id +/- 1}. Note that for music with temporary polyphony,
not all segments will encompass the same moment span. So destination segments
should be filtered to ensure the current moment is in-bounds.

@item
@code{beg-moment <= this-beg-moment < end-moment}. For efficiency, this can be
done simply via @code{assoc} with @code{<=} as the @qq{equality} test, since the
segments are sorted in reverse.

@end enumerate

An editor can inject via @command{-dinclude-settings} a default layout that adds
@code{Record_locations_translator} to @code{Score}. This will enable navigation
data with no changes to user code. The editor can then watch for the creation or
refresh of @file{.nav/*.l}, as well as loading pre-existing nav data on
file-open.

Since @code{Record_locations_translator} supports the export of arbitrary
context properties, in principle this could also be used to enable other
rhythm-aware editor features, such as indentation of mid-measure line breaks.")))

(define-public (count-book-scores book)
  "Recursively count the number of scores in @var{book}."
  (apply + (length (ly:book-scores book))
         (map count-book-scores (ly:book-book-parts book))))

(define (default-toplevel-book-handler book)
  (parameterize ((score-nav-tables (list))
                 (collated-nav-tables (list))
                 (book-score-count (count-book-scores book)))
    (toplevel-book-handler book)
    ;; If there's still un-exported nav-data, export it now
    (when (pair? (score-nav-tables))
      (export-nav-data))))
