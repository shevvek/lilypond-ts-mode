;;; lilypond-ts-scheme-mode.el --- Treesit mode for LilyPond Scheme -*- lexical-binding: t -*-

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

;; A `treesit' major mode for editing GNU LilyPond Scheme files, based on
;; `lilypond-ts-mode'.  Most importantly, this includes support for arbitrarily
;; nested embedded LilyPond code within .scm files.

;; This is mainly aimed at an improved experience for GNU LilyPond development,
;; but if someone were interested, a general Scheme TS mode could probably be
;; factored out of this.

;;; Code:

(require 'lilypond-ts-common)

;;; Grammar installation

(defvar lilypond-ts-scheme-grammar-url
  '("https://github.com/nwhetsell/tree-sitter-lilypond/"
    "main" "lilypond-scheme/src"))

(defun lilypond-ts-scheme--install ()
  (add-to-list 'treesit-language-source-alist
               `(lilypond-scheme . ,lilypond-ts-scheme-grammar-url))
  (treesit-install-language-grammar 'lilypond-scheme))

(unless (treesit-language-available-p 'lilypond-scheme)
  (lilypond-ts-scheme--install))

;;; Mode setup

(defgroup lilypond-ts-scheme nil
  "Customization options for `lilypond-ts-scheme-mode'"
  :group 'lilypond-ts)

;;;###autoload
(define-derived-mode lilypond-ts-scheme-mode lilypond-ts-common-mode
  "LilyPond Scheme"
  "A `treesit' major mode for editing GNU LilyPond Scheme files.

Most features are shared with `lilypond-ts-mode'. Full support for embedded
LilyPond code, including nested embeddings."
  :group 'lilypond-ts-scheme
  (when (treesit-ready-p 'lilypond-scheme)
    (setq-local treesit-primary-parser (treesit-parser-create 'lilypond-scheme))

    (setq-local comment-start ";")
    (setq-local block-comment-start "#!")
    (setq-local block-comment-end "!#")

    (setq-local treesit-font-lock-settings
                (lilypond-ts--construct-font-lock-rules t))

    (setq-local treesit-simple-indent-rules
                `((lilypond-scheme . ,(lilypond-ts--indent-rules))))

    (treesit-major-mode-setup)
    (setq-local syntax-propertize-function
                #'lilypond-ts--propertize-syntax)
    (lilypond-ts-capf-mode 1)
    (lilypond-ts-autodoc-mode 1)))

(derived-mode-set-parent 'lilypond-ts-scheme-mode 'scheme-mode)

(provide 'lilypond-ts-scheme-mode)
;;; lilypond-ts-scheme-mode.el ends here
