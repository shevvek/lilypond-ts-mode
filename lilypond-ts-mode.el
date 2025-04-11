;;; lilypond-ts-mode.el --- Treesit mode for GNU LilyPond -*- lexical-binding: t -*-

;; Copyright (c) 2025 Saul James Tobin

;; Author: Saul James Tobin
;; Version: 0.3-alpha
;; Package-Requires: ((geiser "0.31.1") (geiser-guile "0.28.2") (emacs "30.1"))
;; Keywords: languages, tools, scheme, lilypond, geiser, lisp
;; URL: https://github.com/shevvek/lilypond-ts-mode

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

;; A modern `treesit' major mode for editing GNU LilyPond files, with tight
;; LilyPond REPL integration via `geiser', and "vertical" rhythm-aware
;; navigation. Features include:

;; * Full support for nested Scheme and LilyPond embeddings.
;; * Contextual completion-at-point and autodoc.
;; * Detect and select version-compatible LilyPond installations for compilation
;; and interactive evaluation.
;; * Rhythmic navigation minor mode: easily edit the same beat across all parts.

;; This package also provides `lilypond-ts-scheme-mode', for editing LilyPond
;; Scheme files with support for embedded LilyPond code and most features from
;; `lilypond-ts-mode'.

;;; Code:

(require 'lilypond-ts-common)
(require 'lilypond-ts-navigation)

(defvar-keymap lilypond-ts-mode-map
  :parent lilypond-ts-common-mode-map
  "C-c C-c" #'lilypond-ts-compile-score
  "C-c C-S-c" #'lilypond-ts-compile-parts
  :menu '("LilyPond"
          ["Compile (score)" lilypond-ts-compile-score
           :help "Compile, generating rhythmic navigation metadata."]
          ["Compile (parts)" lilypond-ts-compile-parts
           :help "Compile, without generating rhythmic navigation metadata."]
          "--"))

;;;###autoload
(define-derived-mode lilypond-ts-mode lilypond-ts-common-mode "LilyPond"
  "A modern `treesit' major mode for editing GNU LilyPond files.

Features include:

* Tight LilyPond REPL integration via `geiser'
* \"vertical\" rhythm-aware navigation
* Support for nested Scheme and LilyPond embeddings
* Context-aware completion and autodoc
* Automatic LilyPond version detection and selection, and modular argument sets
* Master file redirection when compiling with LilyPond"
  :group 'lilypond-ts
  (when (treesit-ready-p 'lilypond)
    (setq-local treesit-primary-parser (treesit-parser-create 'lilypond))

    (setq-local comment-start "%")
    (setq-local block-comment-start "%{")
    (setq-local block-comment-end "%}")

    (setq-local treesit-thing-settings lilypond-ts--thing-settings)
    (setq-local treesit-defun-name-function #'lilypond-ts--defun-name)
    (setq-local treesit-defun-tactic 'nested)
    (setq-local treesit-simple-imenu-settings lilypond-ts-imenu-rules)

    (setq-local treesit-font-lock-settings
                (apply #'treesit-font-lock-rules
                       (lilypond-ts--font-lock-rules)))
    (setq-local treesit-font-lock-feature-list lilypond-ts--font-lock-features)

    (treesit-major-mode-setup)
    (setq-local syntax-propertize-function #'lilypond-ts--propertize-syntax)
    (lilypond-ts-autodoc-mode 1)
    (lilypond-ts-capf-mode 1)
    (lilypond-ts-navigation-mode 1)))

(derived-mode-set-parent 'lilypond-ts-mode 'lilypond-mode)
(add-to-list 'auto-mode-alist '("\\.ly\\'" . lilypond-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . lilypond-ts-mode))

(provide 'lilypond-ts-mode)
;;; lilypond-ts-mode.el ends here
