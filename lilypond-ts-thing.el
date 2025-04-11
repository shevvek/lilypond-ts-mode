;;; lilypond-ts-thing.el --- Treesit things and imenu -*- lexical-binding: t -*-

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

;; Strutured navigation support

;;; Code:

(require 'treesit)
(require 'lilypond-ts-utils)

(defsubst lilypond-ts--node-top-level-p (node)
  (treesit-node-match-p (treesit-node-parent node) "lilypond_program"))

(defconst lilypond-ts--scheme-defun-sexp
  '((scheme_list :anchor
                 ((scheme_symbol) @kwd
                  (:match "^define" @kwd))) @def))

(defconst lilypond-ts--defun-query
  (treesit-query-compile 'lilypond
                         `(((assignment_lhs :anchor
                                            (symbol) @name) @def
                                            (:pred lilypond-ts--node-top-level-p
                                                   @def))
                           (((escaped_word) @kwd
                             (:match "^\\\\parserDefine$" @kwd)))
                           ,lilypond-ts--scheme-defun-sexp)))

(defconst lilypond-ts-scheme--defun-query
  (treesit-query-compile 'lilypond-scheme lilypond-ts--scheme-defun-sexp))

(defun lilypond-ts--defun-name (node)
  "For defun node, return the name of the corresponding function or variable.
For assignment_lhs, this will be the text of the symbol at node, otherwise the
text of the next symbol after node."
  (when (treesit-node-match-p node 'defun)
    (let ((start (treesit-node-start node)))
      (treesit-node-text (if (treesit-node-match-p node "assignment_lhs")
                             (treesit-thing-at start 'symbol)
                           (treesit-thing-next (treesit-node-end
                                                (treesit-thing-next start
                                                                    'symbol))
                                               'symbol))
                         t))))

(defvar lilypond-ts--thing-settings
  `((lilypond
     (defun ,(lambda (n)
               (treesit-node-eq
                n (cdar (treesit-query-capture n lilypond-ts--defun-query)))))
     (sexp (or symbol
               ,(regexp-opt '("chord"
                              "property_expression"
                              "named_context"
                              "embedded_scheme"
                              "scheme_embedded_lilypond"
                              "expression_block"
                              "parallel_music"
                              "scheme_list"
                              "scheme_vector"
                              "scheme_byte_vector"))))
     (symbol (or ,(regexp-opt '("escaped_word"
                                "symbol" ;; also matches scheme_symbol
                                "string" ;; also matches scheme_string
                                "scheme_boolean"
                                "scheme_keyword"
                                "scheme_character"
                                "scheme_number"))
                 (,(regexp-opt '("unsigned_integer"
                                 "fraction"
                                 "decimal_number"))
                  . lilypond-ts--node-preceded-by-whitespace)))
     (text ,(regexp-opt '("comment" ;; also matches scheme_comment
                          ))))
    (lilypond-scheme
     (defun ,(lambda (n)
               (treesit-node-eq
                n (cdar (treesit-query-capture n lilypond-ts-scheme--defun-query)))))
     (sexp (or symbol
               ,(regexp-opt '("scheme_embedded_lilypond"
                              "scheme_list"
                              "scheme_vector"
                              "scheme_byte_vector"))))
     (symbol ,(regexp-opt '("scheme_symbol"
                            "scheme_string"
                            "scheme_boolean"
                            "scheme_keyword"
                            "scheme_character"
                            "scheme_number")))
     (text ,(regexp-opt '("scheme_comment"))))))

(defvar lilypond-ts-imenu-rules
  `(("Definitions" defun)
    ("Contexts" "named_context"
     ,(lambda (node)
        (= 4 (treesit-node-child-count node)))
     treesit-node-text)))

(provide 'lilypond-ts-thing)
;;; lilypond-ts-thing.el ends here
