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

;; Except for the configuration settings, this implementation is entirely
;; generic and could be upstreamed into `treesit', similar to the rule-based
;; interfaces for font lock, indentation, imenu, and thing.

;;; Code:

(require 'lilypond-ts-base)
(require 'lilypond-ts-keywords)

;; Query match with no :pred clauses => text regex lists same # of captures
;; Find which capture = node, then apply the rest of each regex list as a "mask"
;; Whichever mask matches fully => capf table
;; Bounds narrowing should mean that only one set of captures is generated, but
;; the mask can "slide" to find a match
;; allow multiple masks to match

(defvar lilypond-ts--treesit-completion-rules nil)
(defvar lilypond-ts--treesit-capf-rules nil)
(defvar lilypond-ts--default-completions-function
  (lambda (key)
    `(lilypond-ts-list ,key)))
(defvar lilypond-ts--capf-predicate-filter
  (lambda (str)
    (string-trim-left str "\\\\")))
(defvar lilypond-ts--capf-properties
  `(:company-docsig
    ,(and geiser-autodoc-use-docsig #'geiser-capf--company-docsig)
    :company-doc-buffer #'geiser-capf--company-doc-buffer
    :company-location #'geiser-capf--company-location))

(defun lilypond-ts--treesit-completion-rule (rule)
  (let* ((key (car rule))
         (plist (cdr rule))
         (get-list (lambda (&rest _)
                     (eval (or (plist-get plist :static-list)
                               (funcall lilypond-ts--default-completions-function
                                        key)))))
         (completions-function (plist-get plist :completions-function))
         (kind (plist-get plist :company-kind)))
    (list key (lambda (str)
                (seq-contains-p (funcall get-list)
                                (funcall lilypond-ts--capf-predicate-filter
                                         str)))
          (lambda (&rest friends)
            (completion-table-dynamic
             (lambda (&optional pfx)
               (let ((completions (apply (or completions-function get-list)
                                         friends)))
                 ;; (when kind
                 ;;   (mapc (lambda (word)
                 ;;           (put-text-property 0 1 :company-kind kind word))
                 ;;         completions))
                 completions)))))))

(defun lilypond-ts--treesit-configure-capf (completion-categories capf-rules)
  (setq lilypond-ts--treesit-completion-rules
        (mapcar #'lilypond-ts--treesit-completion-rule completion-categories))
  (setq lilypond-ts--treesit-capf-rules
        (mapcar
         (lambda (rule)
           `(,(car rule)
             ,(mapcar
               (lambda (mask)
                 (mapcar
                  (lambda (key)
                    (cond
                     ((not key) (list #'always #'ignore))
                     ((or (stringp key)
                          (listp key))
                      (list (lambda (str)
                              (member str (ensure-list key)))
                            (lambda (&rest _)
                              (completion-table-dynamic
                               (lambda (&optional p)
                                 (ensure-list key))))))
                     (t (alist-get key lilypond-ts--treesit-completion-rules
                                   (list #'always #'ignore)))))
                  mask))
               (cadr rule))
             ,@(cddr rule)))
         capf-rules)))

(defun lilypond-ts--treesit-capf ()
  (cl-loop
   with node = (treesit-node-on (1- (point)) (point))
   for (query-pred masks filter) in lilypond-ts--treesit-capf-rules
   for captures = (funcall query-pred node)
   when captures
   thereis (cl-loop
            for mask in masks
            for words = (cl-loop
                         for tail on captures until (> (length mask)
                                                       (length tail))
                         thereis (cl-loop
                                  with home = nil
                                  for (key . neighbor) in tail
                                  for (pred make-table) in mask
                                  for text = (treesit-node-text neighbor t)
                                  unless (treesit-node-eq node neighbor)
                                  if (funcall pred text)
                                  collect text into friends
                                  else return nil end
                                  else do (setf home make-table) end
                                  finally return (apply home friends)))
            when words collect words into tables
            finally return `(,(treesit-node-start node)
                             ,(treesit-node-end node)
                             ,(funcall (or filter #'identity)
                                       (apply #'completion-table-merge
                                              tables))
                             ,@lilypond-ts--capf-properties))))



(defun lilypond-ts--grob-property-completions (&rest friends)
  (let ((tail (seq-drop-while (lambda (friend)
                                (not (seq-contains-p (lilypond-ts-list grobs)
                                                     friend)))
                              friends)))
    (geiser-eval--send/result
     `(:eval (ly:grob-property-completions ,(car tail) ,(cadr tail))))))

;; Reference https://github.com/jdtsmith/kind-icon/blob/main/kind-icon.el
(defvar lilypond-ts--completion-categories
  '((clefs :company-kind "u")
    (repeats :company-kind "cm" :static-list lilypond-ts--repeat-types)
    (pitch-languages :company-kind "e")
    (translators :company-kind "cn")
    (contexts :company-kind "%")
    (grobs :company-kind "c")
    (grob-properties :company-kind "pr"
                     :completions-function lilypond-ts--grob-property-completions)
    (translation-properties :company-kind "pa")
    (context-commands :static-list lilypond-ts--context-property-functions)
    (grob-commands :static-list lilypond-ts--grob-property-functions)
    (lexer-keywords :company-kind "kw" :static-list lilypond-ts--lexer-keywords)
    (markup-functions :company-kind "m")
    (markups :company-kind "s")
    (music-functions :company-kind "f")
    (musics :company-kind "va")
    (context-mods :company-kind "if")
    (output-defs :company-kind "{")
    (context-defs :company-kind "%")
    (scores :company-kind "tx")
    (books :company-kind "rf")
    (music-types :company-kind "ev")
    (music-properties :company-kind "fd")))

(defvar lilypond-ts--capf-rules
  `((,(lambda (node)
        (when (treesit-node-match-p node "escaped_word")
          (list (cons nil node))))
     ((lexer-keywords)
      (musics)
      (music-functions)
      (markups)
      (markup-functions)
      (context-defs)
      (context-mods)
      (output-defs))
     ,(lambda (table)
        (completion-table-subvert table "\\" "")))

    (,(lilypond-ts--treesit-capture-neighborhood
       (treesit-query-compile 'lilypond
                              '(((escaped_word) @0
                                 :anchor
                                 (symbol) @1)))
       1 0 2 'sexp)
     (("\\clef" clefs)
      ("\\repeat" repeats)
      ("\\language" pitch-languages)
      (("\\consists" "\\remove") translators)
      (context-commands contexts)
      (context-commands translation-properties)
      (grob-commands contexts)
      (grob-commands grobs)))

    (,(lilypond-ts--treesit-capture-neighborhood
       (treesit-query-compile 'lilypond
                              '((property_expression
                                 (property_expression
                                  (property_expression :anchor
                                                       (symbol) @0
                                                       (symbol) @1
                                                       :anchor)
                                  (symbol) @2)
                                 (symbol) @3)))
       1 1 4 'sexp)
     ((contexts grobs grob-properties nil)))

    (,(lilypond-ts--treesit-capture-neighborhood
       (treesit-query-compile 'lilypond
                              '((property_expression
                                 (property_expression :anchor
                                                      (symbol) @0
                                                      (symbol) @1
                                                      :anchor)
                                 (symbol) @2)))
       1 1 3 'sexp)
     ((contexts grobs grob-properties)
      (grobs grob-properties nil)))

    (,(lilypond-ts--treesit-capture-neighborhood
       (treesit-query-compile 'lilypond
                              '(((escaped_word) @0 :anchor
                                 (property_expression :anchor
                                                      (symbol) @1
                                                      (symbol) @2
                                                      :anchor))))
       2 1 3 'sexp)
     ((context-commands contexts translation-properties)
      (grob-commands contexts grobs)
      (grob-commands grobs grob-properties)))

    (,(lilypond-ts--treesit-capture-neighborhood
       (treesit-query-compile 'lilypond
                              '((property_expression :anchor
                                                     (symbol) @0
                                                     (symbol) @1
                                                     :anchor)))
       1 1 2 'sexp)
     ((grobs grob-props)
      (contexts grobs)
      (contexts translation-properties)))))

;; Information that needs to be captured:
;; neighborhood ruleset, table, kind, narrowing

(provide 'lilypond-ts-capf)
;;; lilypond-ts-capf.el ends here
