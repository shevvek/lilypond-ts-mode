;;; lilypond-ts-capf.el --- completion at point -*- lexical-binding: t -*-

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

;; Completion at point functions

;;; Code:

(require 'lilypond-ts-base)
(require 'lilypond-ts-keywords)

(defun lilypond-ts--symbol-completions (&optional node)
  (and-let* ((this-node (or node
                            (treesit-node-at (point))))
             (dad (treesit-node-prev-sibling this-node))
             (cmd (and (treesit-node-match-p dad "^escaped_word$")
                       (treesit-node-text dad t))))
    (cond
     ((string-equal cmd "\\clef")
      (lilypond-ts-list clefs))
     ((string-equal cmd "\\repeat")
      lilypond-ts--repeat-types)
     ((string-equal cmd "\\language")
      (lilypond-ts-list pitch-languages))
     ((or (string-equal cmd "\\consists")
          (string-equal cmd "\\remove"))
      (lilypond-ts-list translators)))))

(defun lilypond-ts--symbol-capf (&optional predicate)
  (and-let* (((treesit-parser-list (current-buffer) 'lilypond))
             (this-node (treesit-node-at (point)))
             ((treesit-node-match-p this-node "^symbol$"))
             (start (treesit-node-start this-node))
             (end (treesit-node-end this-node))
             ((< start end))
             (cmps (lilypond-ts--symbol-completions this-node)))
    (list start end
          (completion-table-dynamic (lambda (pfx)
                                      cmps))
          :company-docsig
          (and geiser-autodoc-use-docsig #'geiser-capf--company-docsig)
          :company-doc-buffer #'geiser-capf--company-doc-buffer
          :company-location #'geiser-capf--company-location)))

(defsubst lilypond-ts--context-p (str)
  (seq-contains-p (lilypond-ts-list contexts) str))

(defsubst lilypond-ts--grob-p (str)
  (seq-contains-p (lilypond-ts-list grobs) str))

(defsubst lilypond-ts--grob-property-p (str)
  (seq-contains-p (lilypond-ts-list grob-properties) str))

(defsubst lilypond-ts--translation-property-p (str)
  (seq-contains-p (lilypond-ts-list translation-properties) str))

(defun lilypond-ts--property-completions (&optional node)
  "All valid symbols for the current node within a Lilypond property expression"
  (let* ((this-node (or node
                        (treesit-node-at (point))))
         (parent-node (treesit-node-parent this-node))
         (prop-ex-p (treesit-node-match-p parent-node "^property_expression$"))
         (func-node (treesit-search-forward this-node "^escaped_word$" t))
         (func-text (string-trim-left (treesit-node-text func-node t)
                                      "\\\\"))
         (left-sib (when prop-ex-p (treesit-node-prev-sibling
                                    (treesit-node-prev-sibling this-node))))
         (left-sib (if (treesit-node-match-p left-sib "^property_expression$")
                       (car (last (treesit-node-children left-sib)))
                     left-sib))
         (right-sib (when prop-ex-p (treesit-node-next-sibling
                                     (treesit-node-next-sibling this-node))))
         (left-text (treesit-node-text left-sib t))
         (right-text (treesit-node-text right-sib t))
         (left-ctx-p (when left-sib
                       (lilypond-ts--context-p left-text)))
         (left-grob-p (when (and left-sib
                                 (not left-ctx-p))
                        (lilypond-ts--grob-p left-text)))
         (right-grob-p (when right-sib
                         (lilypond-ts--grob-p right-text)))
         (right-grob-prop-p (when (and right-sib
                                       (not right-grob-p))
                              (lilypond-ts--grob-property-p right-text)))
         (right-ctx-prop-p (when (and right-sib
                                      (not right-grob-p)
                                      (not right-grob-prop-p))
                             (lilypond-ts--translation-property-p right-text))))
    (when lilypond-ts--debug-msgs
      (message "Debug lilypond-ts--property-completions: %s %s %s"
               func-text left-text right-text))
    (when (and (treesit-node-match-p this-node "^symbol$")
               (or prop-ex-p
                   (seq-contains-p lilypond-ts--context-property-functions
                                   func-text)
                   (seq-contains-p lilypond-ts--grob-property-functions
                                   func-text)))
      (append
       (when (and (not left-sib)
                  (or (not right-sib) right-grob-p right-ctx-prop-p))
         (lilypond-ts-list contexts))
       (when (and (not (seq-contains-p lilypond-ts--context-property-functions
                                       func-text))
                  (or (not left-sib) left-ctx-p)
                  (or (not right-sib) right-grob-prop-p))
         (lilypond-ts-list grobs))
       (when left-grob-p
         (geiser-eval--send/result
          `(:eval (ly:grob-property-completions ,left-text ,right-text))))
       (when (and (not (seq-contains-p lilypond-ts--grob-property-functions
                                       func-text))
                  (or (not left-sib) left-ctx-p))
         (lilypond-ts-list translation-properties))))))

(defun lilypond-ts--property-capf (&optional predicate)
  (and-let* (((treesit-parser-list (current-buffer) 'lilypond))
             (this-node (treesit-node-at (point)))
             (this-node (if (treesit-node-match-p this-node "^punctuation$")
                            (treesit-node-at (1- (point)))
                          this-node))
             ((treesit-node-match-p this-node "^symbol$"))
             (start (treesit-node-start this-node))
             (end (treesit-node-end this-node))
             ((< start end))
             (cmps (lilypond-ts--property-completions this-node)))
    (list start end
          (completion-table-dynamic (lambda (pfx)
                                      cmps))
          :company-docsig
          (and geiser-autodoc-use-docsig #'geiser-capf--company-docsig)
          :company-doc-buffer #'geiser-capf--company-doc-buffer
          :company-location #'geiser-capf--company-location)))

(defun lilypond-ts--escaped-word-completions (&optional pfx)
  (let ((pfx (or pfx "")))
    (append
     '("include" "maininput" "version"
       "markup" "markuplist" ;; since these are omitted from lexer-keywords list
       "breve" "longa" "maxima")
     lilypond-ts--lexer-keywords
     (lilypond-ts-list contexts)
     (lilypond-ts-list markup-functions)
     (geiser-eval--send/result `(:eval (keywords-of-type ly:music-word?
                                                         ,pfx))))))

(defun lilypond-ts--escaped-word-capf (&optional predicate)
  (and-let* (((treesit-parser-list (current-buffer) 'lilypond))
             (this-node (treesit-node-at (point)))
             ((treesit-node-match-p this-node "^escaped_word$"))
             (start (1+ (treesit-node-start this-node)))
             (end (treesit-node-end this-node))
             ((< start end)))
    (list start end
          (completion-table-dynamic (lambda (pfx)
                                      (lilypond-ts--escaped-word-completions)))
          :company-docsig
          (and geiser-autodoc-use-docsig #'geiser-capf--company-docsig)
          :company-doc-buffer #'geiser-capf--company-doc-buffer
          :company-location #'geiser-capf--company-location
          :exclusive t)))

(provide 'lilypond-ts-capf)
;;; lilypond-ts-capf.el ends here
