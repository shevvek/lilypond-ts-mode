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

(defun lilypond-ts--fontify-scheme-defun (node override start end &rest _)
  (condition-case err
      (when-let*
          ((kw (treesit-node-get node '((sibling -1 t)
                                        (text t))))
           (name-node (treesit-search-subtree node "scheme_symbol"))
           (default-face
            (if (or (treesit-node-match-p node "scheme_list")
                    (cl-loop for sib = (treesit-node-next-sibling node t)
                             then (treesit-node-next-sibling sib t)
                             always sib
                             while (treesit-node-match-p sib "comment")
                             finally return
                             (when-let ((maybe-lambda (treesit-node-get sib
                                                        '((child 0 t)
                                                          (text t)))))
                               (string-match-p "lambda\\|function"
                                               maybe-lambda))))
                'font-lock-function-name-face
              'font-lock-variable-name-face)))
        (treesit-fontify-with-override (treesit-node-start name-node)
                                       (treesit-node-end name-node)
                                       (pcase kw
                                         ((rx "function")
                                          ;; this is a heuristic for LilyPond
                                          ;; syntax functions, effectively
                                          ;; lambdas with the word "define"
                                          nil)
                                         ((rx (or "syntax" "macro"))
                                          'font-lock-variable-name-face)
                                         ((rx (or "class" "module"
                                                  "library" "record"))
                                          'font-lock-type-face)
                                         (_ default-face))
                                       override start end))
    (error (message "Error fontifying Scheme defun \"%s\" at %s:%d: %S"
                    (treesit-node-text node)
                    (buffer-name)
                    (treesit-node-start node)
                    err)
           nil)))

(defcustom lilypond-ts--scheme-defun-regex
  (rx "define" (or (not alpha) eol))
  "Font lock regex for Scheme defun keywords in `lilypond-ts-mode'."
  :group 'lilypond-ts-font-lock
  :type 'string)

(defcustom lilypond-ts--scheme-kwd-free-regexes
  '("let" "syntax" "lambda" "case" "cond" "match" "regex" "force" "assert"
    "fluid" "wind" "map" "filter" "for-each" "fold" "reduce" "transduce")
  "Scheme font lock keyword regexes for `lilypond-ts-mode' that match only at
the beginning of a symbol (not counting the prefix `ly:')."
  :group 'lilypond-ts-font-lock
  :type '(repeat string))

(defcustom lilypond-ts--scheme-kwd-start-regexes
  '("call" "with" "import" "include" "test" "eval")
  "Scheme font lock keyword regexes for `lilypond-ts-mode' that match anywhere
within a symbol."
  :group 'lilypond-ts-font-lock
  :type '(repeat string))

(defcustom lilypond-ts--scheme-kwds
  '("_i" "G_" "and" "and=>" "any" "apply" "begin" "compose" "const" "cut" "cute"
    "delay" "delete" "delq" "delv" "do" "else" "every" "except" "export"
    "grob-transformer" "guard" "identity" "if" "make" "make-engraver"
    "make-performer" "make-relative" "make-translator" "markup" "memq" "memv"
    "member" "not" "negate" "only" "or" "parameterize" "promise" "receive"
    "remove" "rename" "require-extension" "reverse" "unless" "use-modules"
    "values" "when" "while")
  "Scheme font lock keywords regexes for `lilypond-ts-mode' that must match
exactly (not counting the suffix `!')."
  :group 'lilypond-ts-font-lock
  :type '(repeat string))

(defun lilypond-ts--scheme-keywords-rx ()
  (eval `(rx (or (or (regex ,lilypond-ts--scheme-defun-regex)
                     ,@lilypond-ts--scheme-kwd-free-regexes)
                 (seq bol (? "ly:")
                      (or ,@lilypond-ts--scheme-kwd-start-regexes
                          (seq (or ,@lilypond-ts--scheme-kwds)
                               (? "!") eol)))))))

(defun lilypond-ts--scheme-font-lock-rules ()
  `( :feature scheme-defuns
     ((scheme_list
       :anchor
       ((scheme_symbol) @font-lock-keyword-face
        (:match ,(rx (or (regex lilypond-ts--scheme-defun-regex) "library"))
                @font-lock-keyword-face))
       :anchor
       [(scheme_symbol) (scheme_list)] @lilypond-ts--fontify-scheme-defun))

     :feature scheme-words
     (((scheme_symbol) @font-lock-keyword-face
       (:match ,(lilypond-ts--scheme-keywords-rx) @font-lock-keyword-face)))

     :feature scheme-let
     ((((scheme_symbol) @font-lock-keyword-face
        (:match "let" @font-lock-keyword-face))
       :anchor
       (scheme_symbol) @font-lock-function-name-face))

     :feature scheme-keys
     :override t
     ((scheme_keyword) @font-lock-builtin-face)

     :feature scheme-objects
     :override t
     (((scheme_symbol) @font-lock-type-face
       (:match "^<.+>$" @font-lock-type-face)))

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
     (([(scheme_boolean)
        (scheme_character)] @font-lock-constant-face)
      ;; it looks weird to have ## in different colors
      ((embedded_scheme_prefix) @font-lock-constant-face
       (embedded_scheme_text :anchor [(scheme_boolean)
                                      (scheme_character)])))

     :feature scheme-numbers
     ((scheme_number) @font-lock-number-face)))

(defun lilypond-ts--font-lock-rules ()
  `( :default-language lilypond

     ,@(lilypond-ts--scheme-font-lock-rules)

     :feature comment
     (((comment) @font-lock-comment-face)
      ((scheme_comment) @font-lock-comment-face))

     :feature string
     ((string "\"" @font-lock-string-face)
      (scheme_string "\"" @font-lock-string-face)
      ([(string_fragment)
        (scheme_string_fragment)] @font-lock-string-face)
      ([(escape_sequence)
        (scheme_escape_sequence)] @font-lock-escape-face))

     :feature escaped-word
     ((escaped_word) @font-lock-variable-use-face)

     :feature object
     (((symbol) @font-lock-type-face
       (:pred ,(lilypond-ts--keyword-node-predicate 'contexts 'grobs)
              @font-lock-type-face))
      ((escaped_word) @font-lock-type-face
       (:pred ,(lilypond-ts--keyword-node-predicate 'contexts)
              @font-lock-type-face)))

     :feature object
     :override prepend
     (((symbol) @bold
       (:pred ,(lilypond-ts--keyword-node-predicate 'contexts)
              @bold))
      ((escaped_word) @bold
       (:pred ,(lilypond-ts--keyword-node-predicate 'contexts)
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
       (:pred ,(lilypond-ts--keyword-node-predicate 'markup-functions)
              @font-lock-function-call-face)))

     :feature markup
     :override t
     (((escaped_word) @font-lock-function-call-face
       (:match "^\\\\markup\\(list\\)?$" @font-lock-function-call-face)))

     :feature expression
     :override t
     (((dynamic) @font-lock-builtin-face)
      (((escaped_word) @font-lock-builtin-face
        (:pred ,(lilypond-ts--keyword-node-predicate 'post-events
                                                     'event-functions)
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
       (:pred ,(lilypond-ts--keyword-node-predicate 'lexer-keywords
                                                    'other-keywords)
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
      scheme-keys scheme-defuns scheme-let scheme-objects)
    ( keyword expression object markup
      scheme-words)
    ( number phrasing
      scheme-constants scheme-numbers scheme-fluids)
    ( scheme-predicates scheme-side-effects scheme-punctuation)))

(provide 'lilypond-ts-font-lock)
;;; lilypond-ts-font-lock.el ends here
