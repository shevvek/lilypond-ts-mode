;;; lilypond-ts-scheme-mode.el --- Treesit mode for LilyPond Scheme -*- lexical-binding: t -*-

;; Copyright (c) 2025 Saul James Tobin

;; Author: Saul James Tobin
;; Version: 0.1-alpha
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

;;; Code:

(require 'lilypond-ts-mode)

(defvar lilypond-ts-scheme-grammar-url
  '("https://github.com/shevvek/tree-sitter-lilypond-scheme/"
    "abi-14-nested-embeddings"))

(defun lilypond-ts-scheme--install ()
  (add-to-list 'treesit-language-source-alist
               `(lilypond-scheme . ,lilypond-ts-scheme-grammar-url))
  (treesit-install-language-grammar 'lilypond-scheme))

(unless (treesit-language-available-p 'lilypond-scheme)
  (lilypond-ts-scheme--install))

(defgroup lilypond-ts-scheme nil
  "Customization options for `lilypond-ts-scheme-mode'"
  :group 'lilypond-ts)

(defun lilypond-ts-scheme--propertize-syntax (start end)
  (cl-loop for (a . b) being intervals from start to end property 'treesit-parser
           for embedded-parser = (get-char-property a 'treesit-parser)
           if (and embedded-parser
                   (eq 'lilypond (treesit-parser-language embedded-parser)))
           do (lilypond-ts--propertize-syntax a b)
           else do (put-text-property a b 'syntax-table scheme-mode-syntax-table)))

(defsubst lilypond-ts-scheme--top-level-scheme-p (node)
  (not (treesit-parent-until node "scheme_embedded_lilypond_text" nil)))

(defvar lilypond-ts--scheme-lilypond-range-rule
  '( :embed lilypond
     :host lilypond-scheme
     :local t
     (((scheme_embedded_lilypond_text) @capture
       (:pred lilypond-ts-scheme--top-level-scheme-p @capture)))))

;;;###autoload
(define-derived-mode lilypond-ts-scheme-mode prog-mode "LilyPond Scheme"
  :group 'lilypond-ts-scheme
  (when (and (treesit-ready-p 'lilypond-scheme)
             (treesit-ready-p 'lilypond))
    (setq-local treesit-primary-parser (treesit-parser-create 'lilypond-scheme))
    ;; Recursive directory search takes some time, so only trigger automatically
    ;; if lilypond-ts--lily-installs-alist is empty.
    (unless (multisession-value lilypond-ts--lily-installs-alist)
      (lilypond-ts-find-installs))
    (setq-local geiser-repl-per-project-p lilypond-ts-per-project-repl-p)
    (when lilypond-ts-per-project-repl-p
      ;; Really this should be local per-REPL, not per-buffer, but not worth the
      ;; effort unless there's an empirical performance hit
      (make-local-variable 'lilypond-ts--keywords)
      (setq lilypond-ts--keywords (default-value 'lilypond-ts--keywords)))
    (add-hook 'hack-local-variables-hook #'lilypond-ts--ensure-repl nil t)

    (setq-local comment-start ";")
    (setq-local comment-start-skip "[%;]+{? *")
    (setq-local comment-end "")
    (setq-local block-comment-start "#!")
    (setq-local block-comment-end "!#")

    (setq-local treesit-range-settings
                (apply #'treesit-range-rules
                       lilypond-ts--scheme-lilypond-range-rule))
    (setq-local treesit-language-at-point-function
                (lambda (pos)
                  (if (treesit-parent-until
                       (treesit-node-at pos 'lilypond-scheme)
                       "embedded_lilypond_text")
                      'lilypond
                    'lilypond-scheme)))

    ;; (setq-local treesit-thing-settings lilypond-ts--thing-settings)
    ;; (setq-local treesit-defun-name-function #'lilypond-ts--defun-name)
    ;; (setq-local treesit-defun-tactic 'nested)
    (setq-local treesit-font-lock-settings
                (apply #'treesit-font-lock-rules
                       `( :default-language lilypond-scheme
                          ,@(lilypond-ts--scheme-font-lock-rules)
                          ,@(lilypond-ts--font-lock-rules))))
    (setq-local treesit-font-lock-feature-list lilypond-ts--font-lock-features)
    (setq-local treesit-font-lock-level 3)
    (setq-local treesit-simple-indent-rules lilypond-ts--indent-rules)
    (setq-local treesit--indent-verbose t)
    ;; (setq-local treesit--font-lock-verbose t)
    ;; (setq-local treesit-simple-imenu-settings lilypond-ts-imenu-rules)
    (add-hook 'lilypond-ts-post-eval-hook
              #'lilypond-ts--require-keyword-updates nil t)
    (treesit-major-mode-setup)
    (setq-local lisp-indent-function #'scheme-indent-function)
    ;; to do: set comment-use-syntax
    (setq-local syntax-propertize-function
                #'lilypond-ts-scheme--propertize-syntax)))

(derived-mode-set-parent 'lilypond-ts-scheme-mode 'scheme-mode)
(add-to-list 'auto-mode-alist '("\\.scm\\'" . lilypond-ts-scheme-mode))

(provide 'lilypond-ts-scheme-mode)
;;; lilypond-ts-scheme-mode.el ends here
