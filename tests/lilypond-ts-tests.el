;;; lilypond-ts-tsts.el --- Regression tests -*- lexical-binding: t -*-

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

;;; Code:

(require 'lilypond-ts-mode)
(require 'ert)

(defvar lilypond-ts-test-location
  (file-name-concat lilypond-ts-location "tests"))

(defun lilypond-ts-test--parser-setup ()
  (or (treesit-parser-list (current-buffer) 'lilypond)
      (treesit-parser-create 'lilypond)))

(defun lilypond-ts-test--indent-test ()
  (lilypond-ts-test--parser-setup)
  (setq-local treesit-simple-indent-rules
              (treesit--indent-rules-optimize lilypond-ts-indent-rules))
  (treesit-indent-region (point-min) (point-max)))

(ert-deftest lilypond-ts--indent-tests ()
  (skip-unless (treesit-ready-p 'lilypond))
  (ert-test-erts-file (file-name-concat lilypond-ts-test-location
                                        "lilypond-ts-test--indent.erts")
                      #'lilypond-ts-test--indent-test))

(defun lilypond-ts-test--keyword-setup ()
  (dolist (key lilypond-ts--completion-categories)
    (when-let ((kw-list (intern-soft (concat "lilypond-ts--"
                                             (symbol-name (car key))))))
      (make-local-variable kw-list))))

(defun lilypond-ts-test--capf-test-setup ()
  (lilypond-ts-test--parser-setup)
  (lilypond-ts-test--keyword-setup)
  ;; need to deal with REPL
  (make-local-variable 'lilypond-ts--treesit-capf-rules)
  (make-local-variable 'lilypond-ts--treesit-completion-rules)
  (lilypond-ts-mode))

;; based on elisp--test-completions
(defun lilypond-ts-test--list-completions ()
  (let ((data (lilypond-ts--treesit-capf)))
    (all-completions (buffer-substring (nth 0 data) (nth 1 data))
                     (nth 2 data)
                     (plist-get (nthcdr 3 data) :predicate))))

(ert-deftest lilypond-ts-test--escaped-word-capf ()
  (with-temp-buffer
    (lilypond-ts-test--capf-test-setup)
    (insert "\\tra")
    (let ((comps (lilypond-ts-test--list-completions)))
      (should (member "transpose" comps))
      (should-not (member "transpose-array" comps)))))

(defun lilypond-ts-test--generate-font-lock-assertions (file)
  "Annotate FILE with ERT font lock assertions using FILE's default mode."
  (interactive "f")
  (with-current-buffer (find-file-noselect file)
    (when (featurep 'aggressive-indent)
      (aggressive-indent-mode -1))
    (electric-indent-local-mode -1)
    (goto-char (point-min))
    (while (not (eobp))
      (goto-char (line-end-position))
      (cl-loop with line = (buffer-substring (line-beginning-position)
                                             (line-end-position))
               for (beg . end) being the intervals of line property 'face
               for face = (get-char-property beg 'face line)
               for comment-start = (if (lilypond-ts--scheme-at-p (min (1+ (point))
                                                                      (point-max)))
                                       ";;"
                                     "%%")
               unless (string-blank-p (substring line beg end))
               do (insert (cl-case beg
                            (0 (format "\n%s <- %s" comment-start face))
                            (1 (format "\n %s <- %s" comment-start face))
                            (t (format "\n%s%s^ %s" comment-start
                                       (string-pad "" (- beg 2) ?\s)
                                       face)))))
      (forward-line 1))
    (save-buffer)))

(provide 'lilypond-ts-tests)
;;; lilypond-ts-tests.el ends here
