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
(require 'lilypond-ts-autodoc)

;;; These variables are a generic API but the values are LilyPond specific.

(defvar lilypond-ts--capf-properties
  `( :company-kind lilypond-ts--company-kind
     :company-docsig
     ,(and geiser-autodoc-use-docsig #'lilypond-ts--capf-autodoc)
     :company-doc-buffer geiser-capf--company-doc-buffer
     :company-location geiser-capf--company-location))

(defvar lilypond-ts--treesit-capf-rules nil
  "Internal rules for `lilypond-ts--treesit-capf', compiled with
`lilypond-ts--treesit-capf-setup'.")

(defvar lilypond-ts--treesit-capf-default-matcher
  #'lilypond-ts--match-keyword
  "Function used by `lilypond-ts--treesit-capf-setup' to generate predicates.

This function should accept arguments WORD and KEY, returning non-nil if WORD
belongs to the keyword category denoted by KEY.  For example, it could be
something like:

(lambda (word key)
 (member word (alist-get key keyword-alist)))")

(defvar lilypond-ts--treesit-capf-default-make-table
  #'lilypond-ts--keyword-completion-table
  "Function used by `lilypond-ts--treesit-capf-setup' to generate completions.

This function should accept argument KEY, returning a function like:

(lambda (prefix &rest friends)
  (completion-table-dynamic ...))

Where PREFIX is the string to be completed and FRIENDS are the text of
neighboring `treesit' nodes captured by the capf query.

A simple version could ignore PREFIX and FRIENDS, populating the completion
table via:

(alist-get key keyword-alist)

The FRIENDS argument is available in order to allow for generating completions
in a way that depends on the semantic context.")

;;; These functions are fairly generic

(defsubst lilypond-ts--company-kind (str)
  (get-char-property 0 :company-kind str))

(defun lilypond-ts--keyword-completion-table (key)
  "Return a wrapper for a completion table listing KEY category keywords.

Lookups are performed using `lilypond-ts--get-keywords', in order to trigger
lazy updates via the REPL.  If a category defines :completions-function, the
wrapper runs that with PREFIX and the text of neighboring captured nodes instead
of simply populating the completion table with all words in the category."
  (lambda (&rest friends)
    (completion-table-dynamic
     (lambda (&optional pfx)
       (let* ((keyword-category (lilypond-ts--get-keywords key))
              (completions-function (plist-get keyword-category
                                               :completions-function)))
         (if completions-function
             (apply completions-function pfx friends)
           (cadr keyword-category)))))))

;;; These functions are fully generic.

(defun lilypond-ts--treesit-make-capf-pred (key)
  (cond
   ((not key) #'always)
   ((or (stringp key)
        (listp key))
    (byte-compile (lambda (str)
                    (member str (ensure-list key)))))
   (t (byte-compile
       (lambda (word)
         (funcall lilypond-ts--treesit-capf-default-matcher word key))))))

(defun lilypond-ts--treesit-make-capf-table (key)
  (cond
   ((not key) #'ignore)
   ((or (stringp key)
        (listp key))
    (byte-compile (lambda (&rest _)
                    (completion-table-dynamic
                     (lambda (&optional _p)
                       (ensure-list key))))))
   (t (byte-compile (funcall lilypond-ts--treesit-capf-default-make-table key)))))

(defun lilypond-ts--treesit-capf-setup (capf-rules)
  "Compile CAPF-RULES into a form usable by `lilypond-ts--treesit-capf'.

CAPF-RULES is an alist where each key is a `treesit' language, and each value is
a list of rules for that language.

Each rule is a list of form: (rule-name query masks [post-proc]).

QUERY should be in sexp form, and may match parent or neighbor nodes of the node
to be completed.  During compilation, QUERY is parsed using
`lilypond-ts--treesit-query-depths' to determine the minimum and maximum depth
of capture groups.  `lilypond-ts--treesit-capf' runs queries using
`lilypond-ts--treesit-query-parents'.

MASKS should be a list, where each element MASK is a list describing a single
valid configuration of completion candidates for the nodes captured by QUERY.
Nodes other than the node at point are checked against the corresponding
elements of each MASK.  If they all match, then this is a valid configuration of
words, and completion candidates are retrieved from the MASK element that lines
up with the node at point.

This allows for efficiently narrowing completion candidates based on semantic
context.  For example, Context.GrobName and GrobName.grob-property are valid
configurations of a LilyPond property expression, while GrobName.Context is not,
even though its parse tree is structurally identical. In this case, the QUERY
would match the parse tree configuration without filtering using :match or
:pred.  MASKS would include (context grob) and (grob grob-property) but not
(grob context).  Rules structured in this way are agnostic to which node is
being completed.

Usually, the elements of a MASK should be symbols representing keyword
categories.  In that case, they are matched against neighbor nodes using
`lilypond-ts--treesit-capf-default-matcher', and provide completions for the
node at point using `lilypond-ts--treesit-capf-default-make-table'.  Most
simply, imagine that each symbol is the key for an alist in which the values are
lists of words.

MASK elements can also be a string or list of strings, in which case the
string(s) are the completion candidates and neighbor nodes are matches literally
against them.

Lastly, MASK elements can be nil, in which case neighboring nodes always match,
and completion candidates are empty.

POST-PROC is an optional function that is applied to the final completion
table.  This is the place to employ `completion-table-subvert'.  Note, however,
that in most cases it is more practical to instead use
`lilypond-ts--capf-post-process-alist' to associate post-process functions with
the type of the node at point.

Rules should be ordered from most specific to most general, as only the first
QUERY to match is used.  Completion candidates are used from every matching MASK
with that rule.  If QUERY could match the node at point in multiple possible
capture groups, only the first will be used."
  (cl-loop
   for (language . rules) in capf-rules
   collect (cons
            language
            (cl-loop
             for (rule-name query masks post-proc) in rules
             collect (list
                      rule-name
                      (treesit-query-compile language query)
                      (lilypond-ts--treesit-query-depths query)
                      (lilypond-ts--map-squared
                       (lambda (key)
                         (list (lilypond-ts--treesit-make-capf-pred key)
                               (lilypond-ts--treesit-make-capf-table key)))
                       masks)
                      (byte-compile post-proc))))))

(defun lilypond-ts--treesit-capf ()
  "Generate completions at point by applying `lilypond-ts--treesit-capf-rules'.

The concept is to support smart contextual narrowing of completion suggestions
via a two-step matching algorithm.

Consider the example of a LilyPond property expression: `symbol.symbol'.  There
are various possible sets of words that could fit in either node, but depending
on the text of the other symbol, only certain sets make sense.  For example,
GrobName.grob-property and Context.GrobName are valid, but GrobName.Context is
not valid.  There are further logical dependencies when additional surrounding
nodes are considered.

This situation could be handled via a `treesit' query using :match or :pred, but
consider that in this example there are multiple mutually exclusive sets of
possible valid completion sets for the nodes in question.  To handle this just
with queries would mean constructing a multitude of queries differing only in
their filtering predicate clauses, and during matching running the parse tree
search over and over only to discard the results whenever a predicate failed to
match.  A better design is to run queries purely to capture the configuration of
the parse tree, then iteratively try matching each set of constraints against
the captured nodes to find the valid completions.  Also notice that the
constraints are agnostic to which node it is that needs to be matched.

Each rule is of the form (name query (min-depth . max-depth) masks [post-proc]).

Note that this data format is considered internal.  Rules should be generated
using `lilypond-ts--treesit-capf-setup'.

Each QUERY is run against the MIN-DEPTH to MAX-DEPTH parents of the NODE at or
before point, until a capture is found.  Each MASK represents a consistent set
of completions for nodes captured by QUERY.  We try each MASK in turn, and use
the completions from every MASK that is consistent with the text of the captured
nodes.  Only completions from the first matching query will be returned.  If a
query could match multiple overlapping capture groups containing NODE, only the
first will be used.

To start with, imagine that each element of a mask is just list of words in a
semantic category, which could be used either to match a neighboring node's text
or as completion suggestions.  If MASK has the same length as the number of
captured nodes, we simply check if the text of each captured node (other than
NODE itself) belongs to the corresponding element of MASK.  If so, the mask
fits: we retrieve the completions from whichever element of the mask lined up
with NODE and add them to the completion table.  If MASK is shorter than the
number of captured nodes, we iterate over the cons cells of the capture list,
seeing if the first n nodes match against each mask.  You can picture this as
\"sliding\" MASK along the list of captured nodes until it slots in where each
element of MASK lines up with a matching node.

In reality, mask elements are not simple word lists, but pairs of functions: a
predicate used to check whether the text of a node matches, and a function that
accepts the text of each other node matched by the mask, returning a completion
table.

Once all MASKS have been tried, if the rule includes POST-PROC, it is applied to
the final combined completion table.  This is a good place to apply
`completion-table-subvert', for instance. Completion metadata from
`lilypond-ts--capf-properties' is appended to the capf return value."
  (cl-loop
   with node = (treesit-node-on (1- (point)) (point))
   with language = (treesit-language-at (point))
   for (rule-name query (min-depth . max-depth) masks post-process)
   ;; implicitly this handles the case where there is no treesit parser
   in (cdr (assq language lilypond-ts--treesit-capf-rules))
   for captures = (lilypond-ts--treesit-isolate-capture-group
                   node (lilypond-ts--treesit-query-parents node query
                                                            min-depth max-depth))
   when captures
   thereis (cl-loop
            for mask in masks
            for words = (cl-loop
                         ;; In practice, the most common case should be mask
                         ;; and captures of equal length => only one step.
                         for tail on captures until (> (length mask)
                                                       (length tail))
                         thereis (cl-loop
                                  with home = nil
                                  for neighbor in tail
                                  for (pred make-table) in mask
                                  for text = (treesit-node-text neighbor t)
                                  unless (treesit-node-eq node neighbor)
                                  if (funcall pred text)
                                  collect text into friends
                                  else return nil end
                                  else do (setf home make-table) end
                                  finally return (apply home friends)))
            when words collect words into tables
            finally return
            `(,(treesit-node-start node)
              ,(treesit-node-end node)
              ,(funcall (or post-process
                            (cdr (cl-assoc
                                  node
                                  (cdr (assq language
                                             lilypond-ts--capf-post-process-alist))
                                  :test #'treesit-node-match-p))
                            #'identity)
                        (apply #'completion-table-merge tables))
              . ,lilypond-ts--capf-properties))))

;;; LilyPond specific configuration begins here:

(defvar lilypond-ts--capf-post-process-alist
  `((lilypond
     ("escaped_word" . ,(lambda (table)
                          (completion-table-subvert table "\\" "")))))
  "Alist mapping `treesit' language -> node type -> completion table wrapper.

Used by `lilypond-ts--treesit-capf' to, for example, apply an appropriate
`completion-table-subvert' based the type of node being completed.")

(defvar lilypond-ts--capf-rules
  `((lilypond
     ("escaped_word"
      ((escaped_word) @0)
      ((lexer-keywords)
       (musics)
       (music-functions)
       (markups)
       (markup-functions)
       (context-defs)
       (context-mods)
       (output-defs)))

     ("escaped_word symbol"
      (((escaped_word) @0
        :anchor
        (symbol) @1))
      (("\\clef" clefs)
       ("\\repeat" repeats)
       ("\\language" pitch-languages)
       (("\\consists" "\\remove") translators)
       (("\\context" "\\new") contexts)
       ("\\markupMap" music-types)
       (context-commands contexts)
       (context-commands translation-properties)
       (grob-commands contexts)
       (grob-commands grobs)))

     ("triple property_expression"
      ((property_expression
        (property_expression
         (property_expression :anchor
                              (symbol) @0
                              (symbol) @1
                              :anchor)
         (symbol) @2)
        (symbol) @3))
      ((contexts grobs grob-properties nil)))

     ("double property_expression"
      ((property_expression
        (property_expression :anchor
                             (symbol) @0
                             (symbol) @1
                             :anchor)
        (symbol) @2))
      ((contexts grobs grob-properties)
       (grobs grob-properties nil)))

     ("escaped_word property_expression"
      (((escaped_word) @0 :anchor
        (property_expression :anchor
                             (symbol) @1
                             (symbol) @2
                             :anchor)))
      ((context-commands contexts translation-properties)
       (grob-commands contexts grobs)
       (grob-commands grobs grob-properties)
       ("\\markupMap" music-types music-properties)))

     ("property_expression"
      ((property_expression :anchor
                            (symbol) @0
                            (symbol) @1
                            :anchor))
      ((grobs grob-props)
       (contexts grobs)
       (contexts translation-properties)
       (music-types music-properties)))

     ("paper-block"
      ((((escaped_word) @paper :anchor
         (:match "^\\\\paper$" @paper))
        (expression_block [(symbol) @prop
                           (property_expression :anchor (symbol) @prop)
                           (property_expression
                            (property_expression :anchor (symbol) @prop))
                           (property_expression
                            (property_expression
                             (property_expression :anchor (symbol) @prop)))])))
      ((nil paper-variables)))

     ("symbol"
      ((symbol) @0)
      ;; as a fallback, only suggest completions that start with capital letters
      ;; to avoid suggesting completions for note names by accident
      ((contexts)
       (grobs)
       (translators)
       (music-types)))

     ("scheme_symbol"
      ((scheme_symbol) @0)
      ((scheme-identifiers)
       (grobs)
       (contexts)
       (translators)
       (output-defs)
       (paper-variables)
       (grob-properties)
       (translation-properties)
       (music-types)
       (music-properties)
       (event-classes)
       (stencil-commands)
       (grob-interfaces)))

     ("string_fragment"
      ([(string_fragment) (scheme_string_fragment)] @0)
      ((filename))))
    (lilypond-scheme
     ("scheme_symbol"
      ((scheme_symbol) @0)
      ((scheme-identifiers)
       (grobs)
       (contexts)
       (translators)
       (output-defs)
       (paper-variables)
       (grob-properties)
       (translation-properties)
       (music-types)
       (music-properties)
       (event-classes)
       (stencil-commands)
       (grob-interfaces)))

     ("string_fragment"
      ((scheme_string_fragment) @0)
      ((filename))))))

(define-minor-mode lilypond-ts-capf-mode
  "Capf minor mode for LilyPond."
  :init-value nil
  :lighter "/C"
  (if lilypond-ts-capf-mode
      (progn
        (setq-local lilypond-ts--treesit-capf-rules
                    (lilypond-ts--treesit-capf-setup lilypond-ts--capf-rules))
        (add-hook 'completion-at-point-functions #'lilypond-ts--treesit-capf nil t))
    (remove-hook 'completion-at-point-functions #'lilypond-ts--treesit-capf t)))

(provide 'lilypond-ts-capf)
;;; lilypond-ts-capf.el ends here
