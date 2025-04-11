;;; lilypond-ts-common.el --- Common major mode setup -*- lexical-binding: t -*-

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

;; Common setup code for LilyPond and LilyPond Scheme modes.

;;; Code:

(require 'lilypond-ts-base)
(require 'lilypond-ts-run)
(require 'lilypond-ts-repl)
(require 'lilypond-ts-keywords)
(require 'lilypond-ts-syntax)
(require 'lilypond-ts-thing)
(require 'lilypond-ts-capf)
(require 'lilypond-ts-font-lock)
(require 'lilypond-ts-indent)

;;; Grammar installation

(defvar lilypond-ts-grammar-url
  '("https://github.com/nwhetsell/tree-sitter-lilypond/"
    "tree-sitter-abi-14-main"))

(defun lilypond-ts--install ()
  (add-to-list 'treesit-language-source-alist
               `(lilypond . ,lilypond-ts-grammar-url))
  (treesit-install-language-grammar 'lilypond))

(unless (treesit-language-available-p 'lilypond)
  (lilypond-ts--install))

;;; Keymap

(defvar-keymap lilypond-ts-common-mode-map
  "C-c C-b" #'lilypond-ts-eval-buffer
  "<remap> <geiser-eval-buffer>" #'lilypond-ts-eval-buffer
  "<remap> <eval-buffer>" #'lilypond-ts-eval-buffer
  "C-c C-r" #'lilypond-ts-eval-region
  "<remap> <geiser-eval-region>" #'lilypond-ts-eval-region
  "<remap> <eval-region>" #'lilypond-ts-eval-region
  :menu '("LilyPond"
          ["Eval buffer" lilypond-ts-eval-buffer
           :help "Eval buffer in the LilyPond REPL."]
          ["Eval region" lilypond-ts-eval-region
           :help "Eval region in the LilyPond REPL."]
          ["Restart LilyPond REPL" geiser-repl-restart-repl]
          "--"
          ["Find new LilyPond installs" lilypond-ts-find-installs]
          ["Refresh LilyPond installs" lilypond-ts-refresh-installs]))

;;; Mode-init

(defcustom lilypond-ts-per-project-repl-p t
  "Use a separate LilyPond REPL for each project.

When t, sets `geiser-repl-per-project-p' and makes `lilypond-ts--keywords'
local."
  :group 'lilypond-ts
  :type 'boolean)

(define-derived-mode lilypond-ts-common-mode prog-mode "LilyPond Common"
  :group 'lilypond-ts
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
  (add-hook 'lilypond-ts-post-eval-hook
            #'lilypond-ts--require-keyword-updates nil t)

  (setq-local treesit-thing-settings lilypond-ts--thing-settings)
  (setq-local treesit-defun-name-function #'lilypond-ts--defun-name)
  (setq-local treesit-defun-tactic 'nested)
  (setq-local treesit-simple-imenu-settings lilypond-ts-imenu-rules)

  (setq-local lisp-indent-function #'scheme-indent-function)
  (setq-local treesit-simple-indent-rules (lilypond-ts--indent-rules))

  (setq-local treesit-font-lock-feature-list lilypond-ts--font-lock-features)

  (setq-local comment-start-skip "[%;]+{? *")
  (setq-local comment-end ""))

(provide 'lilypond-ts-common)
;;; lilypond-ts-common.el ends here
