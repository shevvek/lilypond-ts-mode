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

(defun lilypond-ts--scheme-ranges (&optional start end)
  "Find treesit ranges for embedded Scheme and Lilypond blocks, which may be
nested. Flatten them into a list of Scheme ranges that excludes embedded blocks
of Lilypond."
  (let* ((scheme-ranges (treesit-query-range 'lilypond
                                             '((embedded_scheme_text) @capture)
                                             start end))
         (ly-ranges (treesit-query-range 'lilypond
                                         '((scheme_embedded_lilypond) @capture)
                                         start end))
         (scheme-range-bounds (flatten-list scheme-ranges))
         (ly-range-bounds (remq (or end (point-max))
                                (remq (or start (point-min))
                                      (flatten-list ly-ranges))))
         (outer-lily-p (and scheme-range-bounds ly-range-bounds
                            (> (apply #'min scheme-range-bounds)
                               (apply #'min ly-range-bounds))))
         (scheme-stripe-bounds (sort (append scheme-range-bounds
                                             ly-range-bounds
                                             (when outer-lily-p
                                               (list (or start (point-min))
                                                     (or end (point-max))))))))
    (seq-split scheme-stripe-bounds 2)))

(defun lilypond-ts--propertize-syntax (start end)
  (let ((scheme-ranges (lilypond-ts--scheme-ranges start end)))
    (with-silent-modifications
      (put-text-property start end 'syntax-table (syntax-table))
      (dolist (range scheme-ranges)
        ;; unclear why calling scheme-propertize-syntax doesn't work
        ;; maybe it depends on more than just (syntax-table)
        (put-text-property (car range) (cadr range)
                           'syntax-table scheme-mode-syntax-table)))))

(defun lilypond-ts-scheme--propertize-syntax (start end)
  (cl-loop for (a . b) being intervals from start to end property 'treesit-parser
           for embedded-parser = (get-char-property a 'treesit-parser)
           if (and embedded-parser
                   (eq 'lilypond (treesit-parser-language embedded-parser)))
           do (lilypond-ts--propertize-syntax a b)
           else do (put-text-property a b 'syntax-table scheme-mode-syntax-table)))

(defsubst lilypond-ts-scheme--top-level-scheme-p (node)
  (not (treesit-parent-until node "scheme_embedded_lilypond_text" nil)))

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

(defun lilypond-ts-scheme--treesit-language-at (pos)
  (if (treesit-parent-until
       (treesit-node-at pos 'lilypond-scheme)
       "embedded_lilypond_text")
      'lilypond
    'lilypond-scheme))

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
