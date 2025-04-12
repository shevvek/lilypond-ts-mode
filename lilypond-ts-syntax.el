;;; lilypond-ts-syntax.el --- Syntax functions -*- lexical-binding: t -*-

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

;; Syntax propertize and handling for nested embeddings of Scheme/LilyPond.

;; Note that the LilyPond Tree-Sitter grammar includes a full Scheme parser, as
;; embedded Scheme blocks lack a closing delimiter.  Therefore when the primary
;; parser is LilyPond, there is no need for `treesit' ranges.  When the primary
;; parser is Scheme, `treesit' ranges need handle only the first level of
;; embedded LilyPond.  We still need to be able to distinguish Scheme code from
;; LilyPond code regardless of nesting, but we can't use `treesit'
;; language-at-point to do it.

;;; Code:

(require 'lilypond-ts-base)

(defun lilypond-ts--embed-ranges (&optional start end)
  "Find treesit ranges for embedded Scheme and Lilypond blocks, which may be
nested. Flatten them into a list of Scheme ranges that excludes embedded blocks
of Lilypond."
  (let* ((scheme-query '((embedded_scheme_text) @capture))
         (ly-query '((scheme_embedded_lilypond) @capture))
         (parent-node (treesit-node-on start end))
         (scheme-p (eq 'lilypond-scheme (treesit-node-language parent-node)))
         (embed-ranges (treesit-query-range parent-node
                                            (if scheme-p ly-query scheme-query)
                                            start end))
         (host-ranges (treesit-query-range parent-node
                                           (if scheme-p scheme-query ly-query)
                                           start end))
         (embed-range-bounds (flatten-list embed-ranges))
         (host-range-bounds (remq (or end (point-max))
                                  (remq (or start (point-min))
                                        (flatten-list host-ranges))))
         (outer-host-p (and embed-range-bounds host-range-bounds
                            (> (apply #'min embed-range-bounds)
                               (apply #'min host-range-bounds))))
         (embed-stripe-bounds (sort (append embed-range-bounds
                                            host-range-bounds
                                            (when outer-host-p
                                              (list (or start (point-min))
                                                    (or end (point-max))))))))
    (seq-split embed-stripe-bounds 2)))

(defun lilypond-ts--propertize-syntax (start end)
  (let* ((embed-ranges (lilypond-ts--embed-ranges start end))
         (scheme-p (eq 'lilypond-scheme (treesit-parser-language
                                         treesit-primary-parser)))
         (host-table (if scheme-p scheme-mode-syntax-table
                       lilypond-ts-mode-syntax-table))
         (embed-table (if scheme-p lilypond-ts-mode-syntax-table
                        scheme-mode-syntax-table)))
    (with-silent-modifications
      (put-text-property start end 'syntax-table host-table)
      (dolist (range embed-ranges)
        ;; unclear why calling scheme-propertize-syntax doesn't work
        ;; maybe it depends on more than just (syntax-table)
        (put-text-property (car range) (cadr range)
                           'syntax-table embed-table)))))

(defun lilypond-ts--lang-block-parent (node &rest _)
  (treesit-parent-until node (regexp-opt '("embedded_scheme_text"
                                           "scheme_embedded_lilypond"
                                           "lilypond_program"
                                           "scheme_program"))))

(defun lilypond-ts--scheme-at-p (&optional pos)
  (let ((node (treesit-node-on (or pos (point)) (or pos (point)) nil t)))
    (and (treesit-node-match-p node "scheme")
         (not (treesit-node-match-p node (regexp-opt
                                          '("scheme_embedded_lilypond"
                                            "embedded_scheme_prefix")))))))

(defcustom lilypond-ts--scheme-defun-regex
  (rx "define" (or (not alpha) eol))
  "Font lock regex for Scheme defun keywords in `lilypond-ts-mode'."
  :group 'lilypond-ts-font-lock
  :type 'string)

(defsubst lilypond-ts--node-top-level-p (node)
  (treesit-node-match-p (treesit-node-parent node) "lilypond_program"))

(defsubst lilypond-ts--named-defun-p (node)
  (let ((text (treesit-node-text node t)))
    (and (string-match-p lilypond-ts--scheme-defun-regex text)
         (not (string-match-p "lambda\\|function" text)))))

(defun lilypond-ts--scheme-defun-lambda-p (node)
  "Given NODE is the second child of a Scheme defun form, is it a named defun?"
  (or (treesit-node-match-p node "scheme_list")
      (cl-loop for sib = (treesit-node-next-sibling node t)
               then (treesit-node-next-sibling sib t)
               always sib
               while (treesit-node-match-p sib "comment")
               finally return (when-let ((maybe-lambda (treesit-node-get sib
                                                         '((child 0 t)
                                                           (text t)))))
                                (string-match-p "lambda\\|function"
                                                maybe-lambda)))))

(defun lilypond-ts--comment-start-at-point (&optional pos)
  (if (lilypond-ts--scheme-at-p (min (1+ (or pos (point)))
                                     (point-max)))
      ";"
    "%"))

(provide 'lilypond-ts-syntax)
;;; lilypond-ts-syntax.el ends here
