;;; lilypond-ts-font-lock.el --- Font lock rules -*- lexical-binding: t -*-

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

;;; Code:

(require 'lilypond-ts-base)
(require 'lilypond-ts-keywords)

(defun lilypond-ts--fontify-scheme (node override start end &rest _)
  (let ((scheme-ranges (lilypond-ts--scheme-ranges (treesit-node-start node)
                                                   (treesit-node-end node))))
    (dolist (range-interval scheme-ranges)
      (apply #'geiser-syntax--fontify-syntax-region range-interval))))

(defun lilypond-ts--fontify-scheme-defun (node override start end &rest _)
  (let ((kw (treesit-node-get node '((parent 1)
                                     (sibling -1 t)
                                     (text t)))))
    (treesit-fontify-with-override (treesit-node-start node)
                                   (treesit-node-end node)
                                   (pcase kw
                                     ((rx (or "syntax" "macro"))
                                      'font-lock-variable-name-face)
                                     ((rx (or "class" "module"
                                              "library" "record"))
                                      'font-lock-type-name-face)
                                     (_ 'font-lock-function-name-face))
                                   override start end)))

(defun lilypond-ts--scheme-keyword-regexes (level)
  (apply #'append
         (take level `((,(rx "define" (or (not alpha) eol)) "let")
                       ("syntax" "^call" "lambda")
                       ("case" "cond" "^with")
                       ("match" "regex")
                       ("delay" "force")
                       ("^import" "^include")
                       ("^test" "assert")
                       ("^eval" "fluid" "wind")
                       ("map" "filter" "for-each" "fold" "reduce" "transduce")
                       ("^make" "^extract")))))

(defvar lilypond-ts--scheme-keywords
  '("_i" "G_" "and" "and=>" "any" "apply" "begin" "compose" "const" "cut" "cute" "do" "else" "every"
    "except" "export" "grob-transformer" "guard" "identity" "if" "markup" "not" "negate" "only" "or"
    "parameterize" "promise" "receive" "rename" "require-extension" "unless"
    "use-modules" "values" "when" "while"))

(defun lilypond-ts--scheme-font-lock-rules ()
  `( :feature scheme-incantations
     (((scheme_symbol) @font-lock-keyword-face
       (:match ,(eval `(rx (or ,@(lilypond-ts--scheme-keyword-regexes 11)
                               (seq bol (or ,@lilypond-ts--scheme-keywords)
                                    eol))))
               @font-lock-keyword-face)))

     :feature scheme-defuns
     :override t
     ((scheme_list
       :anchor ((scheme_symbol) @font-lock-keyword-face
                (:match ,(rx (or (seq "define" (or (not alpha) eol))
                                 "library"))
                        @font-lock-keyword-face))
       :anchor (scheme_list
                :anchor (scheme_symbol) @lilypond-ts--fontify-scheme-defun))
      (((scheme_symbol) @font-lock-keyword-face
        (:match ,(rx "define" (or (not alpha) eol))
                @font-lock-keyword-face))
       :anchor
       (scheme_symbol) @font-lock-variable-name-face))

     :feature scheme-let
     ((((scheme_symbol) @font-lock-keyword-face
        (:match "let" @font-lock-keyword-face))
       :anchor
       (scheme_symbol) @font-lock-function-name-face))

     :feature scheme-keywords
     :override t
     ((scheme_keyword) @font-lock-builtin-face)

     :feature scheme-objects
     :override t
     (((scheme_symbol) @font-lock-type-name-face
       (:match "^<.+>$" @font-lock-type-name-face)))

     :feature scheme-fluids
     :override t
     (((scheme_symbol) @font-lock-variable-use-face
       (:match ,(rx (or (seq bol "*" (+ anything) "*" eol) "<>"))
               @font-lock-variable-use-face)))

     :feature scheme-side-effects
     :override prepend
     (((scheme_symbol) @bold
       (:match "!$" @bold)))

     :feature scheme-predicates
     :override prepend
     (((scheme_symbol) @italic
       (:match "?$" @italic)))

     :feature scheme-punctuation
     :override prepend
     ((["'" "," "." "`"] @bold)
      ((scheme_quote) @bold
       (:match "^'()$" @bold)))

     :feature scheme-constants
     ([(scheme_boolean)
       (scheme_character)] @font-lock-constant-face)

     :feature scheme-numbers
     ((scheme_number) @font-lock-number-face)))

(defun lilypond-ts--font-lock-rules ()
  `( :default-language lilypond

     ,@(lilypond-ts--scheme-font-lock-rules)
     ;; :feature scheme
     ;; ((embedded_scheme_text) @lilypond-ts--fontify-scheme)

     :feature comment
     (((comment) @font-lock-comment-face)
      ((scheme_comment) @font-lock-comment-face))

     :feature string
     (((string) @font-lock-string-face)
      ((scheme_string) @font-lock-string-face)
      ((string (escape_sequence) @font-lock-escape-face))
      ((scheme_string (scheme_escape_sequence) @font-lock-escape-face)))

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

(defvar lilypond-ts--font-lock-features
  '(( comment string escaped-word
      scheme-keywords scheme-objects)
    ( keyword expression object markup
      scheme-defuns scheme-let)
    ( number phrasing
      scheme-incantations scheme-constants scheme-numbers)
    ( scheme-predicates scheme-side-effects scheme-fluids scheme-punctuation)))

(provide 'lilypond-ts-font-lock)
;;; lilypond-ts-font-lock.el ends here
