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

;;; The values of these variables are LilyPond specific, but the API is generic:

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

;;; This code is entirely generic:

(defvar lilypond-ts--treesit-completion-rules nil)
(defvar lilypond-ts--treesit-capf-rules nil)

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

(defun lilypond-ts--treesit-capf-predicate (pred)
  (cond
   ((not pred) #'ignore)
   ((functionp pred) pred)
   (t (lambda (node)
        (when (treesit-node-match-p node pred)
          (list (cons nil node)))))))

(defun lilypond-ts--treesit-make-capf-table (key)
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

(defun lilypond-ts--treesit-configure-capf (completion-categories capf-rules)
  (setq lilypond-ts--treesit-completion-rules
        (mapcar #'lilypond-ts--treesit-completion-rule completion-categories))
  (setq lilypond-ts--treesit-capf-rules
        (mapcar
         (lambda (rule)
           `(,(funcall #'lilypond-ts--treesit-capf-predicate (car rule))
             ,(mapcar (lambda (mask)
                        (mapcar #'lilypond-ts--treesit-make-capf-table mask))
                      (cadr rule))
             . ,(cddr rule)))
         capf-rules)))

(defun lilypond-ts--treesit-capf ()
  "Generate completions at point by applying `lilypond-ts--treesit-capf-rules',

The concept is to support smart contextual narrowing of completion suggestions
via a two-step matching algorithm.

Consider the example of a LilyPond property expression: `symbol.symbol'. There
are various possible sets of words that could fit in either node, but depending
on the text of the other symbol, only certain sets make sense. For example, if
the first symbol is a grob type, it makes no sense for the second to be a
context property. There are further logical dependencies when additional
surrounding nodes are considered.

This situation could be handled via a `treesit' query using :match or :pred, but
consider that in this example there are multiple mutually exclusive sets of
possible valid completion sets for the nodes in question. To handle this just
with queries would mean constructing a multitude of queries differing only in
their filtering predicate clauses, and during matching running the parse tree
search over and over only to discard the results whenever a predicate failed to
match. A better design is to run queries purely to capture the configuration of
the parse tree, then iteratively try matching each set of constraints against
the captured nodes to find the valid completions. Also notice that the
constraints are agnostic to which node it is that needs to be matched.

Accordingly, each rule consists of a query predicate accepting the node at point
and returning an alist of captured nodes and a list of \"masks\", each
representing one consistent set of completions for one or more nodes captured by
the query predicate. To start with, imagine that each element of a mask is just
list of words in a semantic category, which could be used either to match a
neighboring node's text or as completion suggestions.

It probably only makes sense for the masks to have the same number of elements
as the query's capture groups, but we need to handle the situation where that
doesn't occur, for instance if a query captures optional or quantified nodes.
Also, `treesit' doesn't group nodes captured by the same instance of the query,
so we need to handle the situation where the query predicate returns nodes from
capture groups that aren't even in the right neighborhood.

To do this, we iterate over the cons cells of the capture list, seeing if the
first n nodes match against each mask. You can picture this as \"sliding\" the
mask along the list of captured nodes until it slots in where each element of
the mask lines up with a matching node. If that happens, we retrieve the
completions from whichever element of the mask lined up with the node at point,
add them to the completion table, and go on to check if any other masks match.

In reality, mask elements are not simple word lists, but pairs of functions: a
predicate used to check whether the text of a node matches, and a function that
accepts the text of each other node matched by the mask, returning a completion
table. By default, these functions are set up to encode via lexical closures
equivalent behavior to a simple word list, but this design allows for specific
completion categories to use more complicated contextual logic. In the case of
`lilypond-ts-mode', the text of the grob type node is used to only suggest grob
properties that are valid for that specific grob.

A few last details:

The query predicate is technically not run on the node at point, but the node
spanning the character before point, in order to properly handle completion at
node boundaries.

While all matching masks belonging to the same rule will supply completions,
rules are run in order and completions from only one rule will be returned.

Rules may, in addition to a query predicate and a list of masks, specify a
post-process function. This is applied to the final combined completion table,
and is useful for instance with `completion-table-subvert'.

Completion metadata from `lilypond-ts--capf-properties' is appended to the capf
return value.

Note that the data structures described here are the compiled forms. Users
should generally not directly write mask lambdas."
  (cl-loop
   with node = (treesit-node-on (1- (point)) (point))
   for (query-pred masks post-process) in lilypond-ts--treesit-capf-rules
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
                             ,(funcall (or post-process #'identity)
                                       (apply #'completion-table-merge
                                              tables))
                             . ,lilypond-ts--capf-properties))))

;;; LilyPond specific configuration begins here:

(defun lilypond-ts--grob-property-completions (&rest friends)
  "List all grob properties consistent with grob path elements FRIENDS.

If there are any path elements after the grob type, only list grob properties
that allow nesting."
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
  `(("escaped_word"
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

(provide 'lilypond-ts-capf)
;;; lilypond-ts-capf.el ends here
