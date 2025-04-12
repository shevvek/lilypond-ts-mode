;;; lilypond-ts-indent.el --- Indentation -*- lexical-binding: t -*-

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

;; Treesit indent rules for both LilyPond and LilyPond Scheme.

;;; Code:

(require 'lilypond-ts-syntax)

(defcustom lilypond-ts-indent-offset 2
  "Base indent for `lilypond-ts-mode'."
  :group 'lilypond-ts
  :type 'natnum)
(defcustom lilypond-ts-indent-broken-offset lilypond-ts-indent-offset
  "Indent for line breaks before or after the `=' in a LilyPond expression."
  :group 'lilypond-ts
  :type 'natnum)

(defun lilypond-ts--calculate-scheme-indent (node &rest _)
  (car (ensure-list
        (calculate-lisp-indent
         (treesit-node-start
          (treesit-node-top-level
           node
           ;; This could be simpler if treesit thing predicates allowed 'and
           '("scheme" . (lambda (n)
                          (not (treesit-node-match-p
                                n '(or "scheme_embedded_lilypond"
                                       "scheme_program")))))
           t))))))

(defun lilypond-ts--indent-rules ()
  `(;; Don't indent wrapped strings
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
    ((and (node-is "scheme")
          (not (node-is "embedded_scheme_prefix")))
     ;; calculate-lisp-indent already takes initial indent into account
     column-0
     lilypond-ts--calculate-scheme-indent)

    ;; Base top level indentation
    ((parent-is "program") parent-bol 0)))

(provide 'lilypond-ts-indent)
;;; lilypond-ts-indent.el ends here
