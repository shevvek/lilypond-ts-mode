;;; lilypond-ts-test-batch.el --- Regression tests -*- lexical-binding: t -*-

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

;; Convenience function to batch run regression tests.

;;; Code:

(require 'lilypond-ts-base)
(require 'cl-lib)
(require 'ert)

(defun lilypond-ts-test--run-batch-tests ()
  (interactive)
  (pop-to-buffer "*LilyPond TS Tests*")
  (goto-char (point-max))
  (ert-simple-view-mode)
  (let ((test-process
         (start-process "emacs - lilypond-ts-mode batch tests"
                        "*LilyPond TS Tests*"
                        (expand-file-name invocation-name invocation-directory)
                        "-Q" "--batch"
                        "-L" (cl-find "geiser[^a-zA-Z]+$" load-path
                                      :test #'string-match-p)
                        "-L" (cl-find "geiser-guile[^a-zA-Z]+$" load-path
                                      :test #'string-match-p)
                        "-L" (expand-file-name lilypond-ts-location)
                        "-L" (expand-file-name "tests/" lilypond-ts-location)
                        "-l" "lilypond-ts-tests"
                        "-l" "lilypond-ts-nav-tests"
                        "-f" "ert-run-tests-batch-and-exit")))
    (sit-for 30)
    (when (process-live-p test-process)
      (kill-process test-process))))

(provide 'lilypond-ts-test-batch)
;;; lilypond-ts-test-batch.el ends here
