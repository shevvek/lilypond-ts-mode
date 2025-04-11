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

(defvar lilypond-ts-location
  (file-name-directory (or load-file-name buffer-file-name)))

(defgroup lilypond-ts nil
  "Customization options for `lilypond-ts-mode'"
  :group 'languages)

(defgroup lilypond-ts-font-lock nil
  "Font lock settings for `lilypond-ts-mode'."
  :group 'lilypond-ts)

(provide 'lilypond-ts-base)
;;; lilypond-ts-base.el ends here
