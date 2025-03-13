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

;;; Font lock

(defun lilypond-ts--fontify-scheme (node override start end &rest _)
  (let ((scheme-ranges (lilypond-ts--scheme-ranges (treesit-node-start node)
                                                   (treesit-node-end node))))
    (dolist (range-interval scheme-ranges)
      (apply #'geiser-syntax--fontify-syntax-region range-interval))))

(defun lilypond-ts--font-lock-rules ()
  `(
    :default-language lilypond

    :feature scheme
    ((embedded_scheme_text) @lilypond-ts--fontify-scheme)

    :feature comment
    ((comment) @font-lock-comment-face)

    :feature string
    (((string) @font-lock-string-face)
     ((string (escape_sequence) @font-lock-escape-face)))

    :feature escaped-word
    ((escaped_word) @font-lock-variable-use-face)

    :feature object
    (((symbol) @font-lock-type-face
      (:match ,(eval `(rx bol (or ,@(lilypond-ts-list contexts)
                                  ,@(lilypond-ts-list grobs))
                          eol))
              @font-lock-type-face))
     ((escaped_word) @font-lock-type-face
      (:match ,(eval `(rx bol "\\" (or ,@(lilypond-ts-list contexts))
                          eol))
              @font-lock-type-face)))

    :feature object
    :override prepend
    (((symbol) @bold
      (:match ,(eval `(rx bol (or ,@(lilypond-ts-list contexts))
                          eol))
              @bold))
     ((escaped_word) @bold
      (:match ,(eval `(rx bol "\\" (or ,@(lilypond-ts-list contexts))
                          eol))
              @bold)))

    :feature number
    (([(fraction)
       (decimal_number)] @font-lock-number-face)
     ((unsigned_integer) @bold
      :anchor
      (punctuation ".") @bold :*)
     ((instrument_string_number) @font-lock-number-face))

    :feature number
    :override t
    (((escaped_word) @bold
      (:match ,(rx bol "\\" (or "breve" "longa" "maxima") eol)
              @bold))
     ((punctuation "*") @bold :anchor
      [(fraction)
       (decimal_number)
       (unsigned_integer)] @bold))

    :feature markup
    :override t
    (((escaped_word) @font-lock-function-call-face
      (:match ,(eval `(rx bol "\\" (or "markup" "markuplist"
                                       ,@(lilypond-ts-list markup-functions))
                          eol))
              @font-lock-function-call-face)))

    :feature markup
    :override prepend
    (((escaped_word) @bold
      (:match "^\\\\markup\\(list\\)?$" @bold)))

    :feature expression
    :override t
    (((dynamic) @font-lock-builtin-face)
     (((escaped_word) @font-lock-builtin-face
       (:match  ,(eval `(rx bol (? "\\") ;; optional in order to match \^ and \-
                            (or ,@(lilypond-ts-list post-events)
                                ,@(lilypond-ts-list event-functions))
                            eol))
                @font-lock-builtin-face)))
     ((punctuation ["-" "_" "^"]) @font-lock-builtin-face
      :anchor
      (punctuation ["!" "." "-" "^" "_" ">" "+"]) @font-lock-builtin-face)
     ((punctuation ":") @font-lock-builtin-face
      :anchor
      (unsigned_integer) @font-lock-builtin-face))

    :feature expression
    :override prepend
    (((dynamic) @bold)
     ((punctuation ["-" "_" "^"]) @bold
      :anchor
      (punctuation ["!" "." "-" "^" "_" ">" "+"]) @bold)
     ;; ((escaped_word) @bold
     ;;  (:match  "\\\\[[:punct:]rsmfpz]+" @bold))
     )

    :feature keyword
    :override t
    (((escaped_word) @font-lock-keyword-face
      (:match ,(eval `(rx bol "\\" (or "include" "maininput" "version"
                                       ,@lilypond-ts--lexer-keywords
                                       ,@lilypond-ts--other-keywords)
                          eol))
              @font-lock-keyword-face))
     (((escaped_word) @font-lock-keyword-face
       (:match "\\\\override" @font-lock-keyword-face))
      :anchor [(property_expression)
               (assignment_lhs)])
     ((((escaped_word) @font-lock-keyword-face
        (:match ,(rx bol "\\" "=" eol) @font-lock-keyword-face))
       :anchor
       (unsigned_integer) @font-lock-number-face)))

    :feature phrasing
    :override prepend
    ((punctuation ["\\(" "\\)"]) @font-lock-variable-name-face @bold)
    ))

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
    (setq-local treesit-thing-settings lilypond-ts--thing-settings)
    (setq-local treesit-defun-name-function #'lilypond-ts--defun-name)
    (setq-local treesit-defun-tactic 'nested)
    (setq-local treesit-font-lock-settings
                (apply #'treesit-font-lock-rules
                       (lilypond-ts--font-lock-rules)))
    (setq-local treesit-font-lock-feature-list
                '((comment string escaped-word)
                  (keyword scheme expression object markup)
                  (number phrasing)))
    (setq-local treesit-font-lock-level 3)
    (setq-local treesit-simple-indent-rules lilypond-ts-indent-rules)
    ;; (setq-local treesit--indent-verbose t)
    ;; (setq-local treesit--font-lock-verbose t)
    (setq-local treesit-simple-imenu-settings lilypond-ts-imenu-rules)
    (add-hook 'lilypond-ts-post-eval-hook #'lilypond-ts--require-list-refresh)
    (treesit-major-mode-setup)
    (setq-local lisp-indent-function #'scheme-indent-function)
    (setq-local syntax-propertize-function #'lilypond-ts--propertize-syntax)
    (geiser-autodoc-mode 1)
    (lilypond-ts--treesit-configure-capf lilypond-ts--completion-categories
                                         lilypond-ts--capf-rules)
    (add-hook 'completion-at-point-functions
              #'lilypond-ts--treesit-capf nil t)
    (add-hook 'lilypond-ts-mode-hook #'lilypond-ts--init-nav-watcher)))

(add-to-list 'auto-mode-alist '("\\.ly\\'" . lilypond-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . lilypond-ts-mode))

(provide 'lilypond-ts-mode)
;;; lilypond-ts-mode.el ends here
