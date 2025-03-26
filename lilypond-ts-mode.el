;;; lilypond-ts-mode.el --- Treesit mode for GNU LilyPond -*- lexical-binding: t -*-

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

(require 'lilypond-ts-base)
(require 'lilypond-ts-run)
(require 'lilypond-ts-repl)
(require 'lilypond-ts-keywords)
(require 'lilypond-ts-navigation)
(require 'lilypond-ts-capf)
(require 'lilypond-ts-font-lock)

(defvar lilypond-ts-grammar-url
  '("https://github.com/nwhetsell/tree-sitter-lilypond/" "tree-sitter-abi-14-main"))

(defvar lilypond-ts-location
  (file-name-directory (or load-file-name buffer-file-name)))

(defun lilypond-ts--install ()
  (add-to-list 'treesit-language-source-alist
               `(lilypond . ,lilypond-ts-grammar-url))
  (treesit-install-language-grammar 'lilypond))

(unless (treesit-language-available-p 'lilypond)
  (lilypond-ts--install))

;;; Syntax

(defun lilypond-ts--propertize-syntax (start end)
  (let ((scheme-ranges (lilypond-ts--scheme-ranges start end)))
    (with-silent-modifications
      (put-text-property start end 'syntax-table (syntax-table))
      (dolist (range scheme-ranges)
        ;; unclear why calling scheme-propertize-syntax doesn't work
        ;; maybe it depends on more than just (syntax-table)
        (put-text-property (car range) (cadr range)
                           'syntax-table scheme-mode-syntax-table)))))

;;; Indentation

(defcustom lilypond-ts-indent-offset 2
  "Base indent for `lilypond-ts-mode'."
  :group 'lilypond-ts
  :type 'natnum)
(defcustom lilypond-ts-indent-broken-offset lilypond-ts-indent-offset
  "Indent for line breaks before or after the `=' in a LilyPond expression."
  :group 'lilypond-ts
  :type 'natnum)
(defvar lilypond-ts--indent-rules
  `((lilypond
     ;; Don't indent wrapped strings
     (no-node column-0 0)

     ;; Align braces and brackets with the surrounding scope
     ((node-is "{") parent-bol 0)
     ((node-is "<<") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is ">>") parent-bol 0)

     ;; Indent broken assignments
     ((query (((assignment_lhs) :anchor
               ((punctuation) @equals
                (:match "^=$" @equals)) :anchor
               (_) @rhs)))
      prev-line
      lilypond-ts-indent-broken-offset)

     ;; Indent inside curly braces {}
     ((parent-is "expression_block") parent-bol lilypond-ts-indent-offset)
     ;; Indent inside double angle brackets << >>
     ((parent-is "parallel_music") parent-bol lilypond-ts-indent-offset)
     ;; Indent inside #{ #}
     ((parent-is "scheme_embedded_lilypond") parent-bol lilypond-ts-indent-offset)

     ;; Use scheme-mode indentation for embedded Scheme blocks
     ;; Lilypond embedded within Scheme won't match this rule
     ((lambda (node parent &rest _)
        (treesit-node-match-p (if (treesit-node-check node 'named)
                                  node
                                parent)
                              "scheme"))
      ;; calculate-lisp-indent already takes initial indent into account
      column-0
      (lambda (node &rest _)
        (calculate-lisp-indent (treesit-node-start
                                (lilypond-ts--lang-block-parent node)))))

     ;; Base top level indentation
     ((parent-is "lilypond_program") column-0 0)
     ;; Fallback default
     (catch-all parent 0))))

;;; Keymap

(defvar-keymap lilypond-ts-mode-map
  "C-c C-c" #'lilypond-ts-compile-score
  "C-c C-S-c" #'lilypond-ts-compile-parts
  "C-c C-b" #'lilypond-ts-eval-buffer
  "<remap> <geiser-eval-buffer>" #'lilypond-ts-eval-buffer
  "<remap> <eval-buffer>" #'lilypond-ts-eval-buffer
  "C-c C-r" #'lilypond-ts-eval-region
  "<remap> <geiser-eval-region>" #'lilypond-ts-eval-region
  "<remap> <eval-region>" #'lilypond-ts-eval-region
  :menu '("LilyPond"
          ["Compile (score)" lilypond-ts-compile-score
           :help "Compile, generating rhythmic navigation metadata."]
          ["Compile (parts)" lilypond-ts-compile-parts
           :help "Compile, without generating rhythmic navigation metadata."]
          "--"
          ["Eval buffer" lilypond-ts-eval-buffer
           :help "Eval buffer in the LilyPond REPL."]
          ["Eval region" lilypond-ts-eval-region
           :help "Eval region in the LilyPond REPL."]
          ["Restart LilyPond REPL" geiser-repl-restart-repl]
          "--"
          ["Find new LilyPond installs" lilypond-ts-find-installs]
          ["Refresh LilyPond installs" lilypond-ts-refresh-installs]))

;;; Mode-init

;;;###autoload
(define-derived-mode lilypond-ts-mode prog-mode "LilyPond"
  :group 'lilypond-ts
  (when (treesit-ready-p 'lilypond)
    (setq-local treesit-primary-parser (treesit-parser-create 'lilypond))
    ;; Recursive directory search takes some time, so only trigger automatically
    ;; if lilypond-ts--lily-installs-alist is empty.
    (unless (multisession-value lilypond-ts--lily-installs-alist)
      (lilypond-ts-find-installs))
    (lilypond-ts--ensure-repl)

    (setq-local comment-start "%")
    (setq-local comment-start-skip "[%;]+{? *")
    (setq-local comment-end "")
    (setq-local block-comment-start "%{")
    (setq-local block-comment-end "%}")

    (setq-local treesit-thing-settings lilypond-ts--thing-settings)
    (setq-local treesit-defun-name-function #'lilypond-ts--defun-name)
    (setq-local treesit-defun-tactic 'nested)
    (setq-local treesit-font-lock-settings
                (apply #'treesit-font-lock-rules
                       (lilypond-ts--font-lock-rules)))
    (setq-local treesit-font-lock-feature-list lilypond-ts--font-lock-features)
    (setq-local treesit-font-lock-level 3)
    (setq-local treesit-simple-indent-rules lilypond-ts--indent-rules)
    ;; (setq-local treesit--indent-verbose t)
    ;; (setq-local treesit--font-lock-verbose t)
    (setq-local treesit-simple-imenu-settings lilypond-ts-imenu-rules)
    (add-hook 'lilypond-ts-post-eval-hook #'lilypond-ts--require-list-refresh)
    (treesit-major-mode-setup)
    (setq-local lisp-indent-function #'scheme-indent-function)
    ;; to do: set comment-use-syntax
    (setq-local syntax-propertize-function #'lilypond-ts--propertize-syntax)
    (lilypond-ts-autodoc-mode 1)
    (lilypond-ts-capf-mode 1)
    (lilypond-ts-navigation-mode 1)))

(derived-mode-set-parent 'lilypond-ts-mode 'lilypond-mode)
(add-to-list 'auto-mode-alist '("\\.ly\\'" . lilypond-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . lilypond-ts-mode))

(provide 'lilypond-ts-mode)
;;; lilypond-ts-mode.el ends here
