;;; lilypond-ts-mode.el --- Treesit mode for Lilypond -*- lexical-binding: t -*-

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

(require 'lilypond-ts-base)
(require 'lilypond-ts-run)
(require 'lilypond-ts-repl)
(require 'lilypond-ts-keywords)
(require 'lilypond-ts-navigation)
(require 'lilypond-ts-capf)
(require 'lilypond-ts-font-lock)

(defvar lilypond-ts-grammar-url
  "https://github.com/nwhetsell/tree-sitter-lilypond/")

(defvar lilypond-ts-location
  (file-name-directory (or load-file-name buffer-file-name)))

(defun lilypond-ts--install ()
  (add-to-list 'treesit-language-source-alist
               `(lilypond . (,lilypond-ts-grammar-url)))
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

(defvar lilypond-ts-indent-offset 2)
(defvar lilypond-ts-indent-broken-offset lilypond-ts-indent-offset)
(defvar lilypond-ts-indent-rules
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
     (catch-all parent 0)
     )))

;;; Keymap

(defvar lilypond-ts-mode-map (make-sparse-keymap))
(define-key lilypond-ts-mode-map
            (kbd "C-c C-c") #'lilypond-ts-compile)
(define-key lilypond-ts-mode-map
            [remap eval-buffer] #'lilypond-ts-eval-buffer)
(define-key lilypond-ts-mode-map
            [remap geiser-eval-buffer] #'lilypond-ts-eval-buffer)
(define-key lilypond-ts-mode-map
            (kbd "C-c C-b") #'lilypond-ts-eval-buffer)
(define-key lilypond-ts-mode-map
            [remap eval-region] #'lilypond-ts-eval-region)
(define-key lilypond-ts-mode-map
            [remap geiser-eval-region] #'lilypond-ts-eval-region)
(define-key lilypond-ts-mode-map
            (kbd "C-c C-r") #'lilypond-ts-eval-region)
(define-key lilypond-ts-mode-map
            [remap forward-sentence] #'lilypond-ts-forward-moment)
(define-key lilypond-ts-mode-map
            [remap backward-sentence] #'lilypond-ts-backward-moment)
(define-key lilypond-ts-mode-map
            [remap forward-paragraph] #'lilypond-ts-forward-same-moment)
(define-key lilypond-ts-mode-map
            [remap backward-paragraph] #'lilypond-ts-backward-same-moment)
(define-key lilypond-ts-mode-map
            (kbd "C-c C-n") 'lilypond-ts-set-goal-moment)

;;; Mode-init

(define-derived-mode lilypond-ts-mode prog-mode "Lilypond"
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
    (setq-local treesit-simple-indent-rules lilypond-ts-indent-rules)
    ;; (setq-local treesit--indent-verbose t)
    ;; (setq-local treesit--font-lock-verbose t)
    (setq-local treesit-simple-imenu-settings lilypond-ts-imenu-rules)
    (add-hook 'lilypond-ts-post-eval-hook #'lilypond-ts--require-list-refresh)
    (treesit-major-mode-setup)
    (setq-local lisp-indent-function #'scheme-indent-function)
    ;; to do: set comment-use-syntax
    (setq-local syntax-propertize-function #'lilypond-ts--propertize-syntax)
    (lilypond-ts-autodoc-mode 1)
    (lilypond-ts--treesit-configure-capf lilypond-ts--completion-categories
                                         lilypond-ts--capf-rules)
    (add-hook 'completion-at-point-functions #'lilypond-ts--treesit-capf nil t)
    (add-hook 'lilypond-ts-mode-hook #'lilypond-ts--init-nav-watcher)))

(add-to-list 'auto-mode-alist '("\\.ly\\'" . lilypond-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . lilypond-ts-mode))

(provide 'lilypond-ts-mode)
;;; lilypond-ts-mode.el ends here
