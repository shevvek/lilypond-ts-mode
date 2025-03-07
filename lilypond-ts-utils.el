;;; lilypond-ts-utils.el --- Utility functions -*- lexical-binding: t -*-

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

;; The functions in this package are designed to be as generic as possible, and
;; could potentially be upstreamed into Emacs.

;;; Code:

(defsubst lilypond-ts--overlay-match-p (ov plist)
  "Return non-nil if OV has every property in PLIST with a matching value."
  (cl-loop for (key value) on plist by #'cddr
           always (equal (overlay-get ov key) value)))

(defun lilypond-ts--update-overlay (tick beg end &rest props)
  "Ensure there is an overlay bounded by BEG and END with its :update-tick set
to TICK and properties PROPS. If there is an existing overlay in the interval
BEG to END with all properties and values in PROPS, move it and update its
:update-tick. Otherwise, create a new overlay."
  (let ((overlay (seq-find (lambda (ov)
                             (lilypond-ts--overlay-match-p ov props))
                           (overlays-in beg end))))
    (if overlay
        (move-overlay overlay beg end)
      (setq overlay (make-overlay beg end))
      (cl-loop for (key value) on props by #'cddr
               do (overlay-put overlay key value)
               collect key into keys
               finally (apply #'lilypond-ts--cleanup-overlays
                              tick beg end keys)))
    (overlay-put overlay :update-tick tick)))

(defun lilypond-ts--cleanup-overlays (tick &optional beg end &rest keys)
  "Remove overlays on the current buffer with :update-tick < TICK. Optionally,
only look at overlays between BEG and END. Only look at overlays that include
all properties in KEYS."
  (cl-loop for ov being the overlays
           from (or beg (point-min)) to (or end (point-max))
           for props = (overlay-properties ov)
           for old-tick = (plist-get props :update-tick)
           when (and (not (seq-difference keys props))
                     (number-or-marker-p old-tick)
                     (< old-tick tick))
           do (delete-overlay ov)))

(defun lilypond-ts--go-to-loc (file ln ch &rest col)
  "Open FILE and move point to line LN character CH."
  (when file (find-file file))
  (forward-line (- ln (line-number-at-pos (point))))
  (forward-char ch)
  (point))

(defsubst lilypond-ts--node-preceded-by-whitespace (node)
  (string-match-p "\\s-" (string (char-before (treesit-node-start node)))))

(provide 'lilypond-ts-utils)
;;; lilypond-ts-utils.el ends here
