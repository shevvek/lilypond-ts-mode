;;; lilypond-ts-navigation.el --- Rhythmic navigation -*- lexical-binding: t -*-

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
;; along with lilypond-ts-mode.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A minor mode to enable navigation features that facilitate performing edits
;; to the same rhythmic position across all parts in a score, for example adding
;; or removing measures -- traditionally a tedious task with LilyPond.

;; Currently the key features are navigation commands and a header line
;; displaying rhythmic information.

;; This relies on the injection of a Scheme library during LilyPond compilation,
;; which exports rhythmic metadata for each input location corresponding to a
;; discrete rhythmic event.  The key idea is that LilyPond can export a single
;; location-sorted list of rhythmic events for every context in a score, and the
;; boundaries of separate sequential expressions can be inferred anywhere the
;; rhythmic position of events does not ascend continuously.  Overlays are used
;; for both per-event rhythmic data and for sequential segments, so that
;; navigation features will be useful in modified buffers.  Emacs uses file
;; notification watchers to automatically load rhythmic metadata, which is
;; somewhat prone to OS-specific issues.

;;; Code:

(require 'lilypond-ts-base)
(require 'lilypond-ts-run)
(require 'filenotify)
(require 'thingatpt)

(defvar lilypond-ts--nav-update-tick 0)

(defvar lilypond-ts--watchers nil
  "Active file notification watchers belonging to lilypond-ts-mode.")

(defvar lilypond-ts--nav-data-update-stack nil
  "Alist storing nav data for files not currently open.")

(defvar lilypond-ts--score-id-alist nil
  "Alist storing the list of score-ids with navigation overlays in each file.")

(defvar lilypond-ts--goal-moment-alist nil
  "Alist storing the goal moment for each score-id.

Values are plists with the same format as navigation overlays.")

(defun lilypond-ts--put-moment-overlays (nav-table)
  "Update current buffer with rhythmic metadata overlays from NAV-TABLE.

Since LilyPond input locations are line, char, column rather than byte position,
for efficiency exported navigation data is sorted strictly by input location.
This allows for iterating over NAV-TABLE and over the buffer in only one pass.

Boundaries of sequential music expressions are inferred wherever textually
adjacent rhythmic events are not strictly sequential in musical time.  In
addition to overlays for each discrete rhythmic event, overlays are created to
span each sequential music expression, with :nav-index as the distinguishing
property."
  (save-excursion
    (cl-loop with tick = (setq lilypond-ts--nav-update-tick
                               (1+ lilypond-ts--nav-update-tick))
             with segment-boundary = nil
             with index = 0
             for ((loc1 score-id1 beg-mom1 end-mom1 bar-pos bar-num)
                  (loc2 score-id2 beg-mom2))
             on nav-table
             for last-pt = (setq segment-boundary
                                 (apply #'lilypond-ts--go-to-loc nil loc1))
             then (point)
             for parent-end = (treesit-node-end
                               (treesit-parent-until
                                (treesit-node-at (point)) "expression_block"))
             when loc2 do (apply #'lilypond-ts--go-to-loc nil loc2)
             unless (and loc2
                         (eq score-id1 score-id2)
                         (= end-mom1 beg-mom2))
             do (lilypond-ts--update-overlay tick segment-boundary
                                             (if loc2
                                                 (setq segment-boundary (point))
                                               parent-end)
                                             :score-id score-id1
                                             :nav-index (cl-incf index))
             do (lilypond-ts--update-overlay tick last-pt
                                             (if (< last-pt (point) parent-end)
                                                 (point)
                                               parent-end)
                                             :score-id score-id1
                                             :moment beg-mom1
                                             :end-moment end-mom1
                                             :bar-position bar-pos
                                             :bar-number bar-num)
             unless (memq score-id1 score-ids) collect score-id1 into score-ids
             finally (dolist (id score-ids)
                       (lilypond-ts--cleanup-overlays tick nil nil
                                                      :score-id id)))))

(defun lilypond-ts--refresh-moment-nav (table-alist)
  (cl-loop for (file . table) in (alist-get 'by-input-file table-alist)
           for buffer = (find-buffer-visiting file)
           if buffer do (with-current-buffer buffer
                          (lilypond-ts--put-moment-overlays table))
           else do (push (cons file table) lilypond-ts--nav-data-update-stack))
  (cl-loop for (score-id . files) in (alist-get 'by-score table-alist)
           do (setf (alist-get score-id lilypond-ts--score-id-alist)
                    files)))

(defun lilypond-ts--read-nav-data (fname)
  (with-demoted-errors "Error reading nav data: %S"
    (let ((data-alist (with-temp-buffer
                        (insert-file-contents fname)
                        (read (current-buffer)))))
      (lilypond-ts--refresh-moment-nav data-alist))))

(defun lilypond-ts--nav-watcher-callback (ev)
  (when-let* (((not (eq 'stopped (cadr ev))))
              (ev-file (car (last ev)))
              ((string-equal "l" (file-name-extension ev-file))))
    (message "Reloading navigation data from changed file: %s" ev-file)
    (with-demoted-errors "Error reading navigation data: %S"
      (lilypond-ts--read-nav-data ev-file))))

(defun lilypond-ts--retry-init-watcher-callback (ev)
  (when-let* (((not (eq 'stopped (cadr ev))))
              (ev-file (car (last ev)))
              ((string-match-p ".nav" ev-file)))
    (file-notify-rm-watch (car ev))
    (setf (alist-get ev-file lilypond-ts--watchers nil t)
          nil)
    ;; Unconditionally removing the retry watcher could lead to failure to
    ;; initialize if .nav init fails in some way, but since init-nav-watcher
    ;; can take significant time, this mitigates the risk of a race condition.
    (lilypond-ts--init-nav-watcher ev-file)))

(defun lilypond-ts--init-nav-watcher (&optional fname)
  "Check whether there is already an entry in lilypond-ts--watchers for a .nav
subdirectory in the same folder as the current buffer, or the same folder as
optional arg FNAME. If there is not, read nav data from any .l files in that
.nav folder and initialize a new watcher for that .nav folder, adding it to
lilypond-ts--watchers. If the .nav subdirectory does not exist, watch the
current directory for its creation and try again once it exists.

If a watcher already exists for FNAME, load all nav data for that file from
`lilypond-ts--nav-data-update-stack' and remove those entries."
  (with-demoted-errors "Error initializing lilypond-ts-navigation watcher: %S"
    (if-let* ((fname (or fname (buffer-file-name)))
              (file-dir (file-name-directory fname))
              ((file-exists-p file-dir))
              (nav-dir (file-name-concat file-dir ".nav"))
              ((not (assoc nav-dir lilypond-ts--watchers))))
        (if (file-exists-p nav-dir)
            (prog1
                (push (cons nav-dir
                            (file-notify-add-watch
                             nav-dir '(change)
                             #'lilypond-ts--nav-watcher-callback))
                      lilypond-ts--watchers)
              ;; Reading nav data might result in a call to init-nav-watcher for
              ;; a different file in the same project, hence duplicate watchers
              ;; So do this second. To do: make nav data read-in lazy.
              (mapc #'lilypond-ts--read-nav-data
                    (directory-files nav-dir t "^.*\\.l$")))
          (unless (assoc file-dir lilypond-ts--watchers)
            (push (cons file-dir
                        (file-notify-add-watch
                         file-dir '(change)
                         #'lilypond-ts--retry-init-watcher-callback))
                  lilypond-ts--watchers)))
      (while-let ((nav-table (alist-get fname lilypond-ts--nav-data-update-stack
                                        nil nil #'file-equal-p))
                  (buf (find-buffer-visiting fname)))
        (with-current-buffer buf
          (lilypond-ts--put-moment-overlays nav-table))
        (setf (alist-get fname lilypond-ts--nav-data-update-stack
                         nil t #'file-equal-p)
              nil)))))

(defun lilypond-ts--maybe-remove-nav-watcher (&optional fname)
  "If there are no buffers visiting files in the same directory as the current
file (including that file) with `lilypond-ts-navigation-mode' active, remove any
lilypond-ts--watchers active in that directory. With optional argument FNAME,
use that instead of the current buffer filename."
  (with-demoted-errors "Error removing lilypond-ts-navigation watcher: %S"
    (when-let* ((fname (or fname (buffer-file-name)))
                (file-dir (file-name-directory fname))
                ((cl-loop for b being the buffers
                          when (buffer-local-value lilypond-ts-navigation-mode b)
                          when (buffer-file-name b)
                          never (file-equal-p file-dir (file-name-directory
                                                        (buffer-file-name b)))))
                (nav-dir (file-name-concat file-dir ".nav")))
      (while (or (alist-get file-dir lilypond-ts--watchers
                            nil nil #'file-equal-p)
                 (alist-get nav-dir lilypond-ts--watchers
                            nil nil #'file-equal-p))
        (file-notify-rm-watch (alist-get file-dir lilypond-ts--watchers
                                         nil nil #'file-equal-p))
        (setf (alist-get file-dir lilypond-ts--watchers nil t #'file-equal-p)
              nil)
        (file-notify-rm-watch (alist-get nav-dir lilypond-ts--watchers
                                         nil nil #'file-equal-p))
        (setf (alist-get nav-dir lilypond-ts--watchers nil t #'file-equal-p)
              nil)))))

;;; Navigation commands

(defun lilypond-ts--next-closest-overlay (prop init-pos &optional backward)
  (cl-loop with search-fun = (if backward
                                 #'previous-single-char-property-change
                               #'next-single-char-property-change)
           for pos = init-pos then (funcall search-fun pos prop)
           thereis (cdr (get-char-property-and-overlay pos prop))
           until (if backward
                     (<= pos (point-min))
                   (<= (point-max) pos))))

(defun lilypond-ts--forward-same-moment (&optional backward)
  (save-current-buffer
    (cl-loop
     with score-id = (get-char-property (point) :score-id)
     with m = (or (plist-get (cdr (assq score-id lilypond-ts--goal-moment-alist))
                             :moment)
                  (get-char-property (point) :moment)
                  (get-char-property
                   (previous-single-char-property-change (point) :moment)
                   :moment))
     with score-files = (cdr (assq score-id lilypond-ts--score-id-alist))
     with file-index0 = (seq-position score-files (buffer-file-name))
     with file-count = (length score-files)
     for j from 0 to (1- file-count)
     for file-index = nil then (mod (+ file-index0
                                       (* j (if backward -1 1)))
                                    file-count)
     when file-index do (set-buffer (find-file-noselect
                                     (nth file-index score-files)))
     for start-pos = (if backward
                         (1- (previous-single-char-property-change (point)
                                                                   :moment))
                       (next-single-char-property-change (point) :moment))
     then (if backward (point-max) (point-min))
     thereis (cl-loop
              for pos = start-pos then (if backward (1- beg) end)
              until (if backward
                        (<= pos (point-min))
                      (<= (point-max) pos))
              for segment = (lilypond-ts--next-closest-overlay :nav-index pos
                                                               backward)
              always segment
              for beg = (if backward (overlay-start segment) start-pos)
              then (overlay-start segment)
              for end = (if backward start-pos (overlay-end segment))
              then (overlay-end segment)
              for right-bound = (get-char-property (1- end) :end-moment)
              unless (and right-bound (<= right-bound m))
              when (eq score-id (overlay-get segment :score-id))
              thereis (cl-loop for ov in (overlays-in beg end)
                               for beg-mom = (overlay-get ov :moment)
                               for end-mom = (overlay-get ov :end-moment)
                               until (and beg-mom (< m beg-mom))
                               when (and end-mom (< m end-mom)) return ov)))))

(defun lilypond-ts-up-moment (&optional n)
  "Move to the same musical moment in the next musical expression belonging to
the same score-id. With prefix argument N, move that many musical expressions
forward or, for negative N, backward. If an expression has no music for the
exact moment, move to the nearest earlier moment. Expressions where the moment
is out of bounds will be skipped. If lilypond-ts--goal-moment-alist has a non-nil
value for this score-id, use that moment instead of the moment at point. When
the last music expression is reached, wrap around to the first."
  (interactive "p")
  (dotimes (_ (abs n))
    (when-let ((dest-overlay (lilypond-ts--forward-same-moment (> 0 n)))
               (buf (overlay-buffer dest-overlay)))
      (unless (eq buf (current-buffer))
        (pop-to-buffer buf))
      ;; (goto-char (if backward
      ;;                (overlay-start dest-overlay)
      ;;              (overlay-end dest-overlay)))
      (goto-char (overlay-start dest-overlay))))
  (point))

(defsubst lilypond-ts-down-moment (&optional n)
  "Move to the same musical moment in the previous musical expression. With
prefix argument N, move that many musical expressions backward or, for negative
N, forward. If an expression has no music for the exact moment, move to the
nearest earlier moment. If lilypond-ts--goal-moment-alist has a non-nil value for
this score-id, use that moment instead of the moment at point. When the last
music expression is reached, wrap around to the first."
  (interactive "p")
  (lilypond-ts-up-moment (- n)))

(defun lilypond-ts-set-goal-moment (&optional unset)
  "Update the entry in lilypond-ts--goal-moment-alist for the :score-id at point to
the value of :moment at point, or with prefix argument UNSET to nil. This allows
forward-same-moment and backward-same-moment to cycle through all music
expressions in relation to the same moment, instead of drifting away from the
starting moment whenever an expression lacks music at the exact same moment."
  (interactive "P")
  (when-let ((score-id (get-char-property (point) :score-id)))
    (setf (alist-get score-id lilypond-ts--goal-moment-alist nil t)
          (unless unset
            `( :moment ,(get-char-property (point) :moment)
               :bar-number ,(get-char-property (point) :bar-number)
               :bar-position ,(get-char-property (point) :bar-position))))))

(defun lilypond-ts-forward-moment (&optional n)
  "Move forward to the next musical moment after point in the current music
expression. With prefix argument N, do it N times. For negative arg -N, move
backwards."
  (interactive "p")
  (let ((i 0)
        (backward (< n 0)))
    (while (and (not (if backward (bobp) (eobp)))
                (< i (abs n)))
      (forward-thing-for-char-property :moment backward)
      (when (get-char-property (point) :moment)
        (cl-incf i)))
    (point)))

(defun lilypond-ts-backward-moment (&optional n)
  "Move backward to the next musical moment before point in the current music
expression. With prefix argument N, do it N times. For negative arg -N, move
forwards."
  (interactive "p")
  (lilypond-ts-forward-moment (- n)))

(defun lilypond-ts-forward-measure (&optional n)
  "Move forward to the next musical moment after point in the current music
expression. With prefix argument N, do it N times. For negative arg -N, move
backwards."
  (interactive "p")
  (let* ((i 0)
         (backward (< n 0))
         (search-fun (if backward
                         #'previous-single-char-property-change
                       #'next-single-char-property-change)))
    (while (and (not (if backward (bobp) (eobp)))
                (< i (abs n)))
      (goto-char (funcall search-fun (point) :bar-number))
      (when (get-char-property (point) :bar-number)
        (cl-incf i)))
    (point)))

(defun lilypond-ts-backward-measure (&optional n)
  "Move backward to the next musical moment before point in the current music
expression. With prefix argument N, do it N times. For negative arg -N, move
forwards."
  (interactive "p")
  (lilypond-ts-forward-measure (- n)))

;;; Mode-line/header display

(defgroup lilypond-ts-navigation nil
  "Settings for `lilypond-ts-navigation-mode'."
  :group 'lilypond-ts)

(defcustom lilypond-ts-nav-line-position-format
  "  Position (%1$d : %2$.5s)    "
  "Format string for displaying the current rhythmic position in the header.

Substitution groups 1-3 are available as follows:

  1. Bar number
  2. Bar position
  3. Current moment"
  :group 'lilypond-ts-navigation
  :type 'string)

(defcustom lilypond-ts-nav-line-goal-format
  "    Goal (%1$d : %2$.5s)  "
  "Format string for displaying the current goal moment in the header.

Substitution groups 1-3 are available as follows:

  1. Bar number
  2. Bar position
  3. Goal moment"
  :group 'lilypond-ts-navigation
  :type 'string)

(defcustom lilypond-ts-nav-line-left-span-format
  "(-%1$d : -%2$.5s) -%3$.5s"
  "Format string for displaying the span from current position to goal moment.

Substitution groups 1-3 are available as follows:

  1. Bar number span
  2. Bar position span
  3. Total moment span"
  :group 'lilypond-ts-navigation
  :type 'string)

(defcustom lilypond-ts-nav-line-right-span-format
  "+%3$.5s"
  "Format string for displaying the span from goal to the end of current note.

Currently only the total moment is available, but for consistency it is
substitution group 3.  Substitution groups 1 and 2 are reserved for bar number
and bar position span should that data become available in the future.  For now,
these groups will be nil."
  :group 'lilypond-ts-navigation
  :type 'string)

(defcustom lilypond-ts-nav-line-position-help-format
  "Current position: bar %1$d, remainder %2$.5s"
  "Tooltip format string for the nav line current position display.

Substitution groups 1-3 are available as follows:

  1. Bar number
  2. Bar position
  3. Current moment"
  :group 'lilypond-ts-navigation
  :type 'string)

(defcustom lilypond-ts-nav-line-goal-help-format
  "Navigation goal moment: bar %1$d, remainder %2$.5s"
  "Tooltip format string for the nav line goal moment display.

Substitution groups 1-3 are available as follows:

  1. Bar number
  2. Bar position
  3. Goal moment"
  :group 'lilypond-ts-navigation
  :type 'string)

(defcustom lilypond-ts-nav-line-left-span-help-format
  "From current position to goal: %1$d bars, remainder %2$.5s, total %3$.5s"
  "Tooltip format string for the nav line left duration bar data.

Substitution groups 1-3 are available as follows:

  1. Bar number span
  2. Bar position span
  3. Total moment span"
  :group 'lilypond-ts-navigation
  :type 'string)

(defcustom lilypond-ts-nav-line-right-span-help-format
  "From goal to end of current note or rest: total %3$.5s"
  "Tooltip format string for the nav line right duration bar data.

Currently only the total moment is available, but for consistency it is
substitution group 3.  Substitution groups 1 and 2 are reserved for bar number
and bar position span should that data become available in the future.  For now,
these groups will be nil."
  :group 'lilypond-ts-navigation
  :type 'string)

(defcustom lilypond-ts-nav-line-goal-marker "|G|"
  "String for marking the goal moment position relative to the current note."
  :group 'lilypond-ts-navigation
  :type 'string)

(defcustom lilypond-ts-nav-duration-bar-fill ?\=
  "Fill character for displaying duration of current note relative to goal."
  :group 'lilypond-ts-navigation
  :type 'character)

(defcustom lilypond-ts-nav-duration-bar-ends '("<" . ">")
  "Left and right end strings for the duration bar."
  :group 'lilypond-ts-navigation
  :type '(cons string string))

(defcustom lilypond-ts-nav-line-brackets '("" . "")
  "Left and right brackets to enclose the duration bar section of the nav line.

These are most helpful with `lilypond-ts-nav-line-always-duration-bar' t, to
indicate whether goal moment is within the duration bounds of the current note."
  :group 'lilypond-ts-navigation
  :type '(cons string string))

(defcustom lilypond-ts-nav-line-always-duration-bar nil
  "Toggle display of duration bar current note does not overlap goal moment.

Duration bar will always be shown when the goal moment is within the current
note or rest's duration."
  :group 'lilypond-ts-navigation
  :type 'boolean)

(defface lilypond-ts-duration-bar-left-face
  '((t :inherit diff-added))
  "Face for displaying the left portion of the nav line duration bar."
  :group 'lilypond-ts-navigation)

(defface lilypond-ts-duration-bar-right-face
  '((t :inherit diff-removed))
  "Face for displaying the right portion of the nav line duration bar."
  :group 'lilypond-ts-navigation)

(defface lilypond-ts-duration-bar-goal-face
  nil
  "Face for displaying the goal marker of the nav line duration bar."
  :group 'lilypond-ts-navigation)

(defface lilypond-ts-duration-bar-bracket-face
  nil
  "Face for displaying the brackets around the nav line duration bar."
  :group 'lilypond-ts-navigation)

(defface lilypond-ts-nav-line-position-face
  nil
  "Face for displaying the current rhythmic position in the nav line."
  :group 'lilypond-ts-navigation)

(defface lilypond-ts-nav-line-goal-face
  nil
  "Face for displaying the goal moment in the nav line."
  :group 'lilypond-ts-navigation)

(defcustom lilypond-ts--nav-mode-line-format
  '((:eval (lilypond-ts--rhythmic-position-line)))
  "Header format to display rhythmic position, goal moment, and duration bar."
  :type 'sexp
  :group 'lilypond-ts-navigation)

(defun lilypond-ts--rhythmic-position-line ()
  "Generate header line expression with current position and goal moment.

Current position is displayed on the left, goal moment on the right.

A duration bar is displayed in the center indicating the relationship between
the goal moment and the position and duration of the current note or rest."
  (let* ((curr-bar (get-char-property (point) :bar-number))
         (curr-beat (get-char-property (point) :bar-position))
         (curr-end (get-char-property (point) :end-moment))
         (curr-moment (get-char-property (point) :moment))
         (curr-duration (and curr-moment curr-end
                             (- curr-end curr-moment)))
         (goal-plist (cdr (assq (get-char-property (point) :score-id)
                                lilypond-ts--goal-moment-alist)))
         (goal-bar (plist-get goal-plist :bar-number))
         (goal-beat (plist-get goal-plist :bar-position))
         (goal-moment (plist-get goal-plist :moment))
         (goal-left-duration (and goal-moment curr-moment
                                  (- goal-moment curr-moment)))
         (goal-right-duration (and goal-moment curr-end
                                   (- curr-end goal-moment)))
         (goal-left-bar-diff (and goal-bar curr-bar
                                  (- goal-bar curr-bar)))
         (goal-left-beat-diff (and goal-beat curr-beat
                                   (- goal-beat curr-beat)))
         (curr-string (and curr-bar curr-beat
                           (propertize
                            (format lilypond-ts-nav-line-position-format
                                    curr-bar curr-beat curr-moment)
                            'face 'lilypond-ts-nav-line-position-face
                            'help-echo (format
                                        lilypond-ts-nav-line-position-help-format
                                        curr-bar curr-beat curr-moment))))
         (goal-string (and goal-bar goal-beat
                           (propertize
                            (format lilypond-ts-nav-line-goal-format
                                    goal-bar goal-beat goal-moment)
                            'face 'lilypond-ts-nav-line-goal-face
                            'help-echo
                            (format
                             lilypond-ts-nav-line-goal-help-format
                             goal-bar goal-beat goal-moment))))
         (left-data-string (and goal-left-bar-diff goal-left-beat-diff
                                goal-left-duration (< 0 goal-left-duration)
                                (propertize
                                 (format lilypond-ts-nav-line-left-span-format
                                         goal-left-bar-diff
                                         goal-left-beat-diff goal-left-duration)
                                 'face 'lilypond-ts-duration-bar-left-face
                                 'help-echo
                                 (format
                                  lilypond-ts-nav-line-left-span-help-format
                                  goal-left-bar-diff goal-left-beat-diff
                                  goal-left-duration))))
         (right-data-string (and goal-right-duration (< 0 goal-right-duration)
                                 (propertize
                                  (format lilypond-ts-nav-line-right-span-format
                                          nil nil goal-right-duration)
                                  'face 'lilypond-ts-duration-bar-right-face
                                  'help-echo
                                  (format
                                   lilypond-ts-nav-line-right-span-help-format
                                   nil nil goal-right-duration))))
         (right-align-length (- (window-width) (length goal-string)))
         (pad-width (apply #'- right-align-length
                           (mapcar #'length
                                   (list "  " curr-string
                                         left-data-string
                                         right-data-string
                                         (car lilypond-ts-nav-line-brackets)
                                         (cdr lilypond-ts-nav-line-brackets)
                                         lilypond-ts-nav-line-goal-marker))))
         (left-pad (and left-data-string
                        (min pad-width (round (* pad-width
                                                 (abs goal-left-duration))
                                              curr-duration))))
         (right-pad (and right-data-string (- pad-width (or left-pad 0))))
         (left-bar (and left-pad
                        (propertize
                         (string-pad (car lilypond-ts-nav-duration-bar-ends)
                                     left-pad
                                     lilypond-ts-nav-duration-bar-fill)
                         'face 'lilypond-ts-duration-bar-left-face)))
         (right-bar (and right-pad
                         (propertize
                          (string-pad (cdr lilypond-ts-nav-duration-bar-ends)
                                      right-pad
                                      lilypond-ts-nav-duration-bar-fill t)
                          'face 'lilypond-ts-duration-bar-right-face))))
    (list
     (list right-align-length
           (or curr-string "")
           (when (and (or lilypond-ts-nav-line-always-duration-bar
                          (and left-data-string right-data-string))
                      curr-string goal-string)
             (list
              (when left-data-string
                `(:propertize ,(car lilypond-ts-nav-line-brackets)
                              face lilypond-ts-duration-bar-bracket-face))
              left-data-string
              (unless right-data-string
                `(:propertize ,(cdr lilypond-ts-nav-line-brackets)
                              face lilypond-ts-duration-bar-bracket-face))
              " "
              left-bar
              `(:propertize ,lilypond-ts-nav-line-goal-marker
                            face lilypond-ts-duration-bar-goal-face)
              right-bar
              " "
              (unless left-data-string
                `(:propertize ,(car lilypond-ts-nav-line-brackets)
                              face lilypond-ts-duration-bar-bracket-face))
              right-data-string
              (when right-data-string
                `(:propertize ,(cdr lilypond-ts-nav-line-brackets)
                              face lilypond-ts-duration-bar-bracket-face)))))
     goal-string)))

(defvar-keymap lilypond-ts-navigation-mode-map
  "<remap> <forward-word>" #'lilypond-ts-forward-moment
  "<remap> <backward-word>" #'lilypond-ts-backward-moment
  "<remap> <forward-sentence>" #'lilypond-ts-forward-measure
  "<remap> <backward-sentence>" #'lilypond-ts-backward-measure
  "<remap> <forward-paragraph>" #'lilypond-ts-up-moment
  "<remap> <backward-paragraph>" #'lilypond-ts-down-moment
  "C-c C-n" 'lilypond-ts-set-goal-moment)

(defvar-local lilypond-ts--nav-mode-restore-state nil)

(define-minor-mode lilypond-ts-navigation-mode
  "Minor mode enabling rhythm-aware navigation in LilyPond code.

When enabled, new or refreshed musical metadata will automatically be loaded on
running `lilypond-ts-compile'."
  :init-value nil
  :lighter "/N"
  :group 'lilypond-ts-navigation
  (if lilypond-ts-navigation-mode
      (progn
        (lilypond-ts--init-nav-watcher)
        (setq-local lilypond-ts--nav-mode-restore-state
                    (buffer-local-set-state
                     header-line-format lilypond-ts--nav-mode-line-format)))
    (lilypond-ts--maybe-remove-nav-watcher)
    (buffer-local-restore-state lilypond-ts--nav-mode-line-format)
    (setq-local lilypond-ts--nav-mode-restore-state nil)))

(provide 'lilypond-ts-navigation)
;;; lilypond-ts-navigation.el ends here
