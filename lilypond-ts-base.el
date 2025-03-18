;;; lilypond-ts-base.el --- Treesit mode for Lilypond -*- lexical-binding: t -*-

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

;; Foundational dependencies are collected here

;;; Code:

(require 'treesit)
(require 'scheme)
(require 'cl-lib)
(require 'lilypond-ts-utils)
(require 'lilypond-ts-thing)

(defvar lilypond-ts-location
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar lilypond-ts--debug-msgs nil)

;;; Embedded Scheme

(defun lilypond-ts--scheme-ranges (&optional start end)
  "Find treesit ranges for embedded Scheme and Lilypond blocks, which may be
nested. Flatten them into a list of Scheme ranges that excludes embedded blocks
of Lilypond."
  (let* ((scheme-ranges (treesit-query-range (treesit-buffer-root-node)
                                             '((embedded_scheme_text) @capture)
                                             start end))
         (ly-ranges (treesit-query-range (treesit-buffer-root-node)
                                         '((scheme_embedded_lilypond) @capture)
                                         start end))
         ;; Make a single list with all the range bounds. Since ranges are
         ;; dotted pairs, can't simply concatenate them.
         (boundaries (seq-reduce (lambda (l p)
                                   (cons (cdr p)
                                         (cons (car p) l)))
                                 `(,@scheme-ranges ,@ly-ranges)
                                 nil))
         ;; Since treesit queries automatically expand their bounds to the
         ;; captured node, and since embedded Lilypond will always be inside
         ;; embedded Scheme, it can be assumed that the lowest Scheme bound will
         ;; be less than the lowest Lilypond bound.
         (stripes (seq-split (sort boundaries) 2)))
    stripes))

(defun lilypond-ts--lang-block-parent (node &rest _)
  (treesit-parent-until node (regexp-opt '("embedded_scheme_text"
                                           "scheme_embedded_lilypond"
                                           "lilypond_program"))))

(defun lilypond-ts--scheme-at-p (&optional pos)
  (let ((node (treesit-node-at (or pos (point)) nil t)))
    (and (treesit-node-match-p node "scheme")
         (not (treesit-node-match-p node (regexp-opt
                                          '("scheme_embedded_lilypond"
                                            "embedded_scheme_prefix")))))))

(defun lilypond-ts--comment-start-at-point (&optional pos)
  (if (lilypond-ts--scheme-at-p (min (1+ (or pos (point)))
                                     (point-max)))
      ";"
    "%"))

(provide 'lilypond-ts-base)
;;; lilypond-ts-base.el ends here
