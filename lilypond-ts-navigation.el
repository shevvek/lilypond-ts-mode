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

(require 'lilypond-ts-base)
(require 'lilypond-ts-run)
(require 'filenotify)

(defvar lilypond-ts--nav-update-tick 0)

(defvar lilypond-ts--watchers nil
  "Internal list tracking active file notification watchers belonging to
lilypond-ts-mode.")

(defvar lilypond-ts--moment-navigation-table nil
  "Alist storing the moment navigation table for each score-id. Each table is a
list of alists with elements of form:

((beg-moment . end-moment) (filename line char col) export-props)

Each element alist represents one music expression available to cycle via
forward/backward same-moment, mapping the musical timing of every rhythmic event
within that expression, in reverse order, to the corresponding input location.
The alists are in the same order as expressions appear in the code, or in score
order for expressions in separate files.")

(defvar lilypond-ts--score-id-alist nil)

(defvar lilypond-ts--goal-moments nil
  "Alist storing the goal moment for each score-id.")

(defun lilypond-ts--put-moment-overlays (nav-table)
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
           do (with-current-buffer (find-file-noselect file)
                (lilypond-ts--put-moment-overlays table)))
  (cl-loop for (score-id . files) in (alist-get 'by-score table-alist)
           do (setf (alist-get score-id lilypond-ts--score-id-alist)
                    files)))

(defun lilypond-ts--read-nav-data (fname)
  ;;(with-demoted-errors "Error reading nav data: %S"
  (let ((data-alist (with-temp-buffer
                      (insert-file-contents fname)
                      (read (current-buffer)))))
    (lilypond-ts--refresh-moment-nav data-alist)))

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
current directory for its creation and try again once it exists."
  (with-demoted-errors "Error initializing lilypond-ts-navigation watcher: %S"
    (when-let* ((fname (or fname (buffer-file-name)))
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
                lilypond-ts--watchers))))))

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

;; Go to beginning/end of overlay depending on = or < moment
(defun lilypond-ts-forward-same-moment (&optional n)
  "Move to the same musical moment in the next musical expression belonging to
the same score-id. With prefix argument N, move that many musical expressions
forward or, for negative N, backward. If an expression has no music for the exact
moment, move to the nearest earlier moment. Expressions where the moment is out
of bounds will be skipped. If lilypond-ts--goal-moments has a non-nil value for
this score-id, use that moment instead of the moment at point. When the last
music expression is reached, wrap around to the first."
  (interactive "p")
  (and-let* ((pos (point))
             (score-id (get-char-property pos :score-id))
             (this-moment (or (alist-get score-id lilypond-ts--goal-moments)
                              (get-char-property pos :moment)))
             (index (get-char-property pos :index))
             (nav-table (alist-get score-id
                                   lilypond-ts--moment-navigation-table))
             (rotated-table (append (seq-drop nav-table index)
                                    (seq-take nav-table index)))
             (bounded-table (seq-filter (lambda (t)
                                          (and (< this-moment (cdaar t))
                                               (<= (caaar (last t)) this-moment)))
                                        rotated-table))
             ((length> bounded-table 1))
             (dest-index (mod n (length bounded-table)))
             (dest-nav-alist (nth dest-index bounded-table))
             (dest (cl-assoc this-moment dest-nav-alist :key #'car :test #'>=)))
    (apply #'lilypond-ts--go-to-loc (cadr dest))))

(defun lilypond-ts--next-segment (score-id &optional initial-pos backward)
  (cl-loop with search-fun = (if backward
                                 #'previous-single-char-property-change
                               #'next-single-char-property-change)
           for pos = (or initial-pos (point)) then (if ov
                                                       (if backward
                                                           (overlay-start ov)
                                                         (overlay-end ov))
                                                     (funcall search-fun
                                                              pos :nav-index))
           for (index . ov) = (get-char-property-and-overlay pos :nav-index)
           until (or (>= (point-min) pos)
                     (<= (point-max) pos))
           when (and index ov (if backward
                                  (< (overlay-end ov) (point))
                                (< (point) (overlay-start ov)))
                     (eq score-id (overlay-get ov :score-id)))
           return ov))

(defun lilypond-ts--forward-same-moment (&optional backward)
  (cl-loop with score-id = (get-char-property (point) :score-id)
           with score-files = (cdr (assq score-id lilypond-ts--score-id-alist))
           with m = (or (cdr (assq score-id lilypond-ts--goal-moments))
                        (get-char-property (point) :moment))
           with j0 = (seq-position score-files (buffer-file-name))
           with jn = (length score-files)
           for j from 0 to (1- jn)
           for segment = (lilypond-ts--next-segment score-id nil backward)
           then (with-current-buffer
                    (find-file-noselect
                     (nth (mod (+ j0 (* j (if backward -1 1))) jn)
                          score-files))
                  (lilypond-ts--next-segment score-id (point-min) backward))
           for beg = (when segment (overlay-start segment))
           for end = (when segment (overlay-end segment))
           for buf = (when segment (overlay-buffer segment))
           until (and segment
                      (eq buf (current-buffer))
                      (<= beg (point))
                      (< (point) end))
           when (and segment
                     (<= (get-char-property beg :moment buf) m)
                     (< m (get-char-property end :end-moment buf)))
           thereis (cl-find m (overlays-in beg end)
                            :key (lambda (ov)
                                   (or (overlay-get ov :moment)
                                       (1+ m)))
                            :test #'>= :from-end t)))

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
     with m = (or (cdr (assq score-id lilypond-ts--goal-moments))
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
              when (eq score-id (overlay-get segment :score-id))
              thereis (cl-find-if (lambda (ov)
                                    (when-let
                                        ((m-beg (overlay-get ov :moment))
                                         (m-end (overlay-get ov :end-moment)))
                                      (and (<= m-beg m)
                                           (< m m-end))))
                                  (overlays-in beg end))))))

(defun lilypond-ts-up-moment (&optional backward)
  (interactive "P")
  (when-let ((dest-overlay (lilypond-ts--forward-same-moment backward))
             (buf (overlay-buffer dest-overlay)))
    (unless (eq buf (current-buffer))
      (pop-to-buffer buf))
    ;; (goto-char (if backward
    ;;                (overlay-start dest-overlay)
    ;;              (overlay-end dest-overlay)))
    (goto-char (overlay-start dest-overlay))))

(defsubst lilypond-ts-backward-same-moment (&optional n)
  "Move to the same musical moment in the previous musical expression. With
prefix argument N, move that many musical expressions backward or, for negative
N, forward. If an expression has no music for the exact moment, move to the
nearest earlier moment. If lilypond-ts--goal-moments has a non-nil value for
this score-id, use that moment instead of the moment at point. When the last
music expression is reached, wrap around to the first."
  (interactive "p")
  (lilypond-ts-forward-same-moment (- n)))

(defun lilypond-ts-set-goal-moment (&optional unset)
  "Update the entry in lilypond-ts--goal-moments for the :score-id at point to
the value of :moment at point, or with prefix argument UNSET to nil. This allows
forward-same-moment and backward-same-moment to cycle through all music
expressions in relation to the same moment, instead of drifting away from the
starting moment whenever an expression lacks music at the exact same moment."
  (interactive "P")
  (when-let ((score-id (get-char-property (point) :score-id)))
    (setf (alist-get score-id lilypond-ts--goal-moments nil t)
          (unless unset (get-char-property (point) :moment)))))

(defun lilypond-ts-forward-moment (&optional n)
  "Move forward to the next musical moment after point in the current music
expression. With prefix argument N, do it N times. For negative arg -N, move
backwards."
  (interactive "p")
  (and-let* ((pos (point))
             (this-moment (get-char-property pos :moment))
             (score-id (get-char-property pos :score-id))
             (voice-index (get-char-property pos :index))
             (score-table (alist-get score-id
                                     lilypond-ts--moment-navigation-table))
             (my-table (nth voice-index score-table))
             (moment-index (seq-position my-table this-moment
                                         (lambda (elt now)
                                           (= (caar elt) now))))
             ;; Subtract n because the moment list is backwards
             (dest-index (- moment-index n))
             (max-index (1- (length my-table)))
             (dest-loc (nth (if (< max-index dest-index) max-index dest-index)
                            my-table)))
    (apply #'lilypond-ts--go-to-loc (cadr dest-loc))))

(defsubst lilypond-ts-backward-moment (&optional n)
  "Move backward to the next musical moment before point in the current music
expression. With prefix argument N, do it N times. For negative arg -N, move
forwards."
  (interactive "p")
  (lilypond-ts-forward-moment (- n)))

(defvar-keymap lilypond-ts-navigation-mode-map
  "<remap> <forward-sentence>" #'lilypond-ts-forward-moment
  "<remap> <backward-sentence>" #'lilypond-ts-backward-moment
  "<remap> <forward-paragraph>" #'lilypond-ts-forward-same-moment
  "<remap> <backward-paragraph>" #'lilypond-ts-backward-same-moment
  "C-c C-n" 'lilypond-ts-set-goal-moment)

(define-minor-mode lilypond-ts-navigation-mode
  "Minor mode enabling rhythm-aware navigation in LilyPond code.

When enabled, new or refreshed musical metadata will automatically be loaded on
running `lilypond-ts-compile'."
  :init-value nil
  :lighter "/N"
  (if lilypond-ts-navigation-mode
      (lilypond-ts--init-nav-watcher)
    (lilypond-ts--maybe-remove-nav-watcher)))

(provide 'lilypond-ts-navigation)
;;; lilypond-ts-navigation.el ends here
