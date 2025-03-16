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

(provide 'lilypond-ts-tests)
;;; lilypond-ts-tests.el ends here
