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

(defvar lilypond-ts--goal-moments nil
  "Alist storing the goal moment for each score-id.")

(defun lilypond-ts--put-moment-overlays (nav-table)
  (save-excursion
    (cl-loop with tick = (setq lilypond-ts--nav-update-tick
                               (1+ lilypond-ts--nav-update-tick))
             for (((ln1 ch1) score-id index moment) ((ln2 ch2))) on nav-table
             for last-pt = (lilypond-ts--go-to-loc nil ln1 ch1) then (point)
             for parent-end = (treesit-node-end
                               (treesit-parent-until
                                (treesit-node-at (point)) "expression_block"))
             when ln2 do (lilypond-ts--go-to-loc nil ln2 ch2)
             do (lilypond-ts--update-overlay tick last-pt
                                             (if (< last-pt (point) parent-end)
                                                 (point)
                                               parent-end)
                                             :moment moment
                                             :index index
                                             :score-id score-id)
             unless (memq score-id score-ids) collect score-id into score-ids
             finally (dolist (id score-ids)
                       (lilypond-ts--cleanup-overlays tick nil nil
                                                      :moment :index
                                                      :score-id id)))))

(defun lilypond-ts--refresh-moment-nav (table-alist)
  (cl-loop for (file . table) in (alist-get 'by-input-file table-alist)
           do (with-current-buffer (find-file-noselect file)
                (lilypond-ts--put-moment-overlays table)))
  (cl-loop for (score-id . table) in (alist-get 'by-score table-alist)
           do (setf (alist-get score-id
                               lilypond-ts--moment-navigation-table)
                    table)))

(defun lilypond-ts--read-nav-data (fname)
  (let ((data-alist (with-temp-buffer
                      (insert-file-contents fname)
                      (read (current-buffer)))))
    (lilypond-ts--refresh-moment-nav data-alist)))

(defun lilypond-ts--nav-watcher-callback (ev)
  (let ((ev-file (car (last ev))))
    (when (string-equal "l" (file-name-extension ev-file))
      (message "Reloading navigation data from changed file: %s" ev-file)
      (with-demoted-errors "Error reading navigation data: %S"
        (lilypond-ts--read-nav-data ev-file)))))

(defun lilypond-ts--init-nav-watcher (&optional fname)
  "Check whether there is already an entry in lilypond-ts--watchers for a .nav
subdirectory in the same folder as the current buffer, or the same folder as
optional arg FNAME. If there is not, read nav data from any .l files in that
.nav folder and initialize a new watcher for that .nav folder, adding it to
lilypond-ts--watchers. If the .nav subdirectory does not exist, watch the
current directory for its creation and try again once it exists."
  (when-let* ((fname (or fname (buffer-file-name)))
              (file-dir (file-name-directory fname))
              (nav-dir (file-name-concat file-dir ".nav"))
              ((not (assoc nav-dir lilypond-ts--watchers))))
    (if (file-exists-p nav-dir)
        (progn
          (mapc #'lilypond-ts--read-nav-data
                (directory-files nav-dir t "^.*\\.l$"))
          (push (cons nav-dir
                      (file-notify-add-watch nav-dir '(change)
                                             #'lilypond-ts--nav-watcher-callback))
                lilypond-ts--watchers))
      (push (cons file-dir
                  (file-notify-add-watch
                   file-dir '(change)
                   (lambda (ev)
                     (let ((ev-file (car (last ev))))
                       (when (string-match-p ".nav"
                                             ev-file)
                         (lilypond-ts--init-nav-watcher file-dir)
                         (setf (alist-get file-dir
                                          lilypond-ts--watchers nil t)
                               nil))))))
            lilypond-ts--watchers))))

(defun lilypond-ts--maybe-remove-nav-watcher (&optional fname)
  "If there are no buffers visiting files in the same directory as the current
file (including that file) with `lilypond-ts-navigation-mode' active, remove any
lilypond-ts--watchers active in that directory. With optional argument FNAME,
use that instead of the current buffer filename."
  (when-let* ((fname (or fname (buffer-file-name)))
              (file-dir (file-name-directory fname))
              ((cl-loop for b being the buffers
                        when (buffer-local-value lilypond-ts-navigation-mode b)
                        when (buffer-file-name b)
                        never (file-equal-p file-dir (file-name-directory
                                                      (buffer-file-name b)))))
              (nav-dir (file-name-concat file-dir ".nav")))
    (file-notify-rm-watch (alist-get file-dir lilypond-ts--watchers))
    (setf (alist-get file-dir lilypond-ts--watchers nil t) nil)
    (file-notify-rm-watch (alist-get nav-dir lilypond-ts--watchers))
    (setf (alist-get nav-dir lilypond-ts--watchers nil t) nil)))

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
