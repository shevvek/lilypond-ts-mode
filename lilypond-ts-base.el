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

(defgroup lilypond-ts nil
  "Customization options for `lilypond-ts-mode'"
  :group 'languages)

;;; Embedded Scheme

(defun lilypond-ts--scheme-ranges (&optional start end)
  "Find treesit ranges for embedded Scheme and Lilypond blocks, which may be
nested. Flatten them into a list of Scheme ranges that excludes embedded blocks
of Lilypond."
  (let* ((scheme-ranges (treesit-query-range 'lilypond
                                             '((embedded_scheme_text) @capture)
                                             start end))
         (ly-ranges (treesit-query-range 'lilypond
                                         '((scheme_embedded_lilypond) @capture)
                                         start end))
         (scheme-range-bounds (flatten-list scheme-ranges))
         (ly-range-bounds (remq (or end (point-max))
                                (remq (or start (point-min))
                                      (flatten-list ly-ranges))))
         (outer-lily-p (and scheme-range-bounds ly-range-bounds
                            (> (apply #'min scheme-range-bounds)
                               (apply #'min ly-range-bounds))))
         (scheme-stripe-bounds (sort (append scheme-range-bounds
                                             ly-range-bounds
                                             (when outer-lily-p
                                               (list (or start (point-min))
                                                     (or end (point-max))))))))
    (seq-split scheme-stripe-bounds 2)))

(defun lilypond-ts--lang-block-parent (node &rest _)
  (treesit-parent-until node (regexp-opt '("embedded_scheme_text"
                                           "scheme_embedded_lilypond"
                                           "lilypond_program"
                                           "scheme_program"))))

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
