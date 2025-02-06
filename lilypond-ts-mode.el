;;; lilypond-ts-mode.el --- Treesit mode for Lilypond -*- lexical-binding: t -*-

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'treesit)
(require 'scheme)
(require 'geiser-lilypond-guile)

(defvar lilypond-ts-grammar-url
  "https://github.com/nwhetsell/tree-sitter-lilypond/")

(defvar lilypond-ts-location
  (file-name-directory (or load-file-name buffer-file-name)))

(defun lilypond-ts--install ()
  (add-to-list 'treesit-language-source-alist
               `(lilypond . (,lilypond-ts-grammar-url)))
  (treesit-install-language-grammar 'lilypond))

(unless (treesit-language-available-p 'lilypond)
  (lilypond-ts--install))

;;; Options

(defvar lilypond-ts--debug-msgs nil)

;;; Things

(defsubst lilypond-ts--node-preceded-by-whitespace (node)
  (string-match-p "\\s-" (string (char-before (treesit-node-start node)))))

(defsubst lilypond-ts--node-top-level-p (node)
  (treesit-node-match-p (treesit-node-parent node) "lilypond_program"))

(defvar lilypond-ts--defun-query
  (treesit-query-compile 'lilypond
                         '(((assignment_lhs :anchor
                                            (symbol) @name) @def
                                            (:pred lilypond-ts--node-top-level-p
                                                   @def))
                           (((escaped_word) @kwd
                             (:match "^\\\\parserDefine$" @kwd)))
                           ((scheme_list :anchor
                                         ((scheme_symbol) @kwd
                                          (:match "^define" @kwd))) @def))))

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
               list
               ,(regexp-opt '("chord"
                              "property_expression"
                              "named_context"
                              "embedded_scheme"
                              "scheme_embedded_lilypond"))))
     (list ,(regexp-opt '("expression_block"
                          "parallel_music"
                          "scheme_list"
                          "scheme_vector"
                          "scheme_byte_vector")))
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
                          ))))))

(defvar lilypond-ts-imenu-rules
  `(("Definitions" defun)
    ("Contexts" "named_context"
     ,(lambda (node)
        (= 4 (treesit-node-child-count node)))
     treesit-node-text)))

;;; Embedded Scheme

(defun lilypond-ts--scheme-ranges (&optional start end)
  "Find treesit ranges for embedded Scheme and Lilypond blocks, which may be
nested. Flatten them into a list of Scheme ranges that excludes embedded blocks
of Lilypond."
  (let* ((scheme-ranges (treesit-query-range (treesit-buffer-root-node)
                                             '((embedded_scheme_text) @capture)
                                             start end))
         (ly-ranges (treesit-query-range (treesit-buffer-root-node)
                                         '((scheme_embedded_lilypond) @capture)
                                         start end))
         ;; Make a single list with all the range bounds. Since ranges are
         ;; dotted pairs, can't simply concatenate them.
         (boundaries (seq-reduce (lambda (l p)
                                   (cons (cdr p)
                                         (cons (car p) l)))
                                 `(,@scheme-ranges ,@ly-ranges)
                                 nil))
         ;; Since treesit queries automatically expand their bounds to the
         ;; captured node, and since embedded Lilypond will always be inside
         ;; embedded Scheme, it can be assumed that the lowest Scheme bound will
         ;; be less than the lowest Lilypond bound.
         (stripes (seq-split (sort boundaries) 2)))
    stripes))

(defun lilypond-ts--propertize-syntax (start end)
  (let ((scheme-ranges (lilypond-ts--scheme-ranges start end)))
    (with-silent-modifications
      (put-text-property start end 'syntax-table (syntax-table))
      (dolist (range scheme-ranges)
        ;; unclear why calling scheme-propertize-syntax doesn't work
        ;; maybe it depends on more than just (syntax-table)
        (put-text-property (car range) (cadr range)
                           'syntax-table scheme-mode-syntax-table)))))

(defun lilypond-ts--lang-block-parent (node &rest _)
  (treesit-parent-until node
                        (lambda (n)
                          (string-match-p (rx (or "embedded_scheme_text"
                                                  "scheme_embedded_lilypond"
                                                  "lilypond_program"))
                                          (treesit-node-type n)))))

;;; Indentation

(defvar lilypond-ts-indent-offset 2)
(defvar lilypond-ts-indent-broken-offset lilypond-ts-indent-offset)
(defvar lilypond-ts-indent-rules
  `((lilypond
     ;; Don't indent wrapped strings
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
     ((lambda (node &rest _)
        (string-equal "embedded_scheme_text"
                      (treesit-node-type (lilypond-ts--lang-block-parent node))))
      ;; calculate-lisp-indent already takes initial indent into account
      column-0
      (lambda (node &rest _)
        (calculate-lisp-indent (treesit-node-start
                                (lilypond-ts--lang-block-parent node)))))

     ;; Base top level indentation
     ((parent-is "lilypond_program") column-0 0)
     ;; Fallback default
     (catch-all parent 0)
     )))

;;; Keyword lists

(defun lilypond-ts--get-and-maybe-refresh (plist &optional force-refresh)
  (when-let* (((or force-refresh
                   (plist-get plist :needs-update)))
              ((featurep 'geiser-lilypond-guile))
              ((ly-guile--ensure-repl))
              (scm-code (plist-get plist :scm))
              (new-data (geiser-eval--send/result `(:eval ,scm-code)))
              (wrap-element (or (plist-get plist :wrap-element)
                                #'identity)))
    (plist-put plist :needs-update nil)
    (plist-put plist :value (delq nil (mapcar wrap-element new-data))))
  (plist-get plist :value))

(defvar lilypond-ts-post-eval-hook
  nil)

(defvar lilypond-ts--lists-to-refresh
  nil)

(defun lilypond-ts--require-list-refresh ()
  (dolist (plist lilypond-ts--lists-to-refresh)
    (plist-put plist :needs-update t)))

(defmacro lilypond-ts-list (suffix)
  (let ((plist-name (intern (concat "lilypond-ts--" (symbol-name suffix)))))
    `(progn
       (add-to-list 'lilypond-ts--lists-to-refresh ,plist-name)
       (lilypond-ts--get-and-maybe-refresh ,plist-name))))

(defvar lilypond-ts--lexer-keywords
  '(;; extracted from lily-lexer.cc
    ;; override is removed as it is contextually a markup command
    ;; highlight "markup" and "markuplist" as a markup
    "accepts" "addlyrics" "alias"  "alternative" "book" "bookpart" "change"
    "chordmode" "chords" "consists" "context" "default" "defaultchild" "denies"
    "description" "drummode" "drums" "etc" "figuremode" "figures" "header"
    "layout" "lyricmode" "lyrics" "lyricsto" "midi" "name" "new" "notemode"
    "paper" "remove" "repeat" "rest" "revert" "score" "sequential" "set"
    "simultaneous" "tempo" "type" "unset" "with"))

(defvar lilypond-ts--other-keywords
  nil)

(setq lilypond-ts--other-keywords
      '("absolute" "acciaccatura" "after" "afterGrace" "alterBroken"
        "appendToTag" "applyContext" "applyMusic" "applyOutput" "appoggiatura"
        "autoChange" "cadenzaOff" "cadenzaOn" "compoundMeter"
        "contextPropertyCheck" "cueDuring" "cueDuringWithClef" "fixed" "grace"
        "hide" "keepWithTag" "language" "languageRestore"
        "languageSaveAndChange" "markupMap" "omit" "once" "ottava"
        "overrideProperty" "parallelMusic" "partCombine" "partial"
        "popContextProperty" "propertyOverride" "propertyRevert" "propertySet"
        "propertyTweak" "propertyUnset" "pushContextProperty" "pushToTag"
        "quoteDuring" "relative" "removeWithTag" "scaleDurations" "settingsFrom"
        "single" "slashedGrace" "stopStaff" "tag" "tagGroup" "temporary" "time"
        "times" "transpose" "transposedCueDuring" "transposition" "tuplet"
        "tweak" "undo" "unfoldRepeats" "unfolded" "void" "volta"))

(defvar lilypond-ts--contexts
  '( :value nil
     :needs-update t
     :scm (ly:all-context-names)
     :wrap-element symbol-name))

(defvar lilypond-ts--grobs
  '( :value nil
     :needs-update t
     :scm (map car all-grob-descriptions)
     :wrap-element symbol-name))

(defvar lilypond-ts--translators
  '( :value nil
     :needs-update t
     :scm (map ly:translator-name (ly:get-all-translators))
     :wrap-element symbol-name))

(defvar lilypond-ts--markup-functions
  '( :value nil
     :needs-update t
     :scm (keywords-of-type (lambda (v)
                              (or (markup-function? v)
                                  (markup-list-function? v))))
     :wrap-element (lambda (sym)
                     (string-trim-right (symbol-name sym)
                                        "-markup\\(-list\\)?"))))

(defvar lilypond-ts--post-events
  '( :value nil
     :needs-update t
     :scm (keywords-of-type ly:event?)
     :wrap-element symbol-name))

(defvar lilypond-ts--event-functions
  '( :value nil
     :needs-update t
     :scm (keywords-of-type ly:event-function?)
     :wrap-element symbol-name))

(defvar lilypond-ts--grob-properties
  '( :value nil
     :needs-update t
     :scm all-backend-properties
     :wrap-element symbol-name))

(defvar lilypond-ts--translation-properties
  '( :value nil
     :needs-update t
     :scm all-translation-properties
     :wrap-element symbol-name))

(defvar lilypond-ts--music-types
  '( :value nil
     :needs-update t
     :scm (map car music-descriptions)
     :wrap-element symbol-name))

(defvar lilypond-ts--music-properties
  '( :value nil
     :needs-update t
     :scm all-music-properties
     :wrap-element symbol-name))

(defvar lilypond-ts--pitch-languages
  '( :value nil
     :needs-update t
     :scm (map car language-pitch-names)
     :wrap-element symbol-name))

(defvar lilypond-ts--clefs
  '( :value nil
     :needs-update t
     :scm (map car (filter pair? supported-clefs))
     :wrap-element identity))

;;; Font lock

(defun lilypond-ts--fontify-scheme (node override start end &rest _)
  (when (featurep 'geiser-lilypond-guile)
    (let ((scheme-ranges (lilypond-ts--scheme-ranges (treesit-node-start node)
                                                     (treesit-node-end node))))
      (dolist (range-interval scheme-ranges)
        (apply #'geiser-syntax--fontify-syntax-region range-interval)))))

(defun lilypond-ts--font-lock-rules ()
  `(
    :default-language lilypond

    :feature comment
    ((comment) @font-lock-comment-face)

    :feature string
    (((string) @font-lock-string-face)
     ((string (escape_sequence) @font-lock-escape-face)))

    :feature escaped-word
    ((escaped_word) @font-lock-variable-use-face)

    :feature scheme
    ((embedded_scheme_text) @lilypond-ts--fontify-scheme)

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

;;; Completion

(defvar lilypond-ts--repeat-types
  '(unfold tremolo volta segno percent))

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

;; Candidates to add to this list can be queried by running:
;; (keywords-of-type ly:accepts-maybe-property-path?) in the Geiser REPL
(defvar lilypond-ts--context-property-functions
  '("contextPropertyCheck" "popContextProperty" "propertySet" "propertyUnset"
    "pushContextProperty" "set" "unset"))

(defvar lilypond-ts--grob-property-functions
  ;; \footnote is excluded since the grob path argument is after arguments that
  ;; are likely to include \-escaped words, so \footnote won't be found by
  ;; searching backward from the property expression to the first escaped_word.
  ;; Note that some of these accept only a grob path, not a grob-property path.
  ;; Currently, grob property completions will still be offered if the user adds
  ;; a . following the grob name.
  '("alterBroken" "applyOutput" "hide" "offset" "omit" "override"
    "overrideProperty" "parenthesize" "propertyOverride" "propertyRevert"
    "propertyTweak" "revert" "shape" "styledNoteHeads" "tweak" "vshape"))

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

;;; Eval

(defun lilypond-ts-eval-region (start end)
  "Async eval the region within the current Geiser LilyPond REPL."
  (interactive "r")
  (let* ((start-node (treesit-node-at start))
         (end-node (treesit-node-at end))
         (start-lang-block (lilypond-ts--lang-block-parent start-node))
         (end-lang-block (lilypond-ts--lang-block-parent end-node))
         (one-lang-parent-p (treesit-node-eq start-lang-block end-lang-block))
         (start (treesit-node-start start-node))
         (end (treesit-node-end end-node)))
    (cond
     ((not one-lang-parent-p)
      (message "lilypond-ts-eval-region error: start and end node do not belong to the same language block."))
     ((treesit-node-match-p start-lang-block "embedded_scheme_text")
      (geiser-eval-region start end)
      (run-hooks 'lilypond-ts-post-eval-hook))
     (t (geiser-eval--send
         `(:eval (ly:parser-parse-string
                  (ly:parser-clone)
                  ,(buffer-substring-no-properties start end)))
         (lambda (s)
           (message "%s" (geiser-eval--retort-result-str s nil))))
        (run-hooks 'lilypond-ts-post-eval-hook)))))

(defun lilypond-ts-eval-buffer (&optional buffer)
  "Async eval a LilyPond buffer within the current Geiser LilyPond REPL."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (fname (expand-file-name (buffer-file-name buf))))
    (with-current-buffer buf
      (if (file-exists-p fname)
          (geiser-eval--send
           `(:eval (ly:parser-parse-string
                    (ly:parser-clone)
                    ,(format "\\include \"%s\"" fname)))
           (lambda (s)
             (run-hooks 'lilypond-ts-post-eval-hook)
             (message "%s" s)))
        (lilypond-ts-eval-region (point-min) (point-max))))))

;;; Mode-init

(define-derived-mode lilypond-ts-mode prog-mode "Lilypond"
  (when (treesit-ready-p 'lilypond)
    (setq-local treesit-primary-parser (treesit-parser-create 'lilypond))
    (setq-local treesit-thing-settings lilypond-ts--thing-settings)
    (setq-local treesit-defun-name-function #'lilypond-ts--defun-name)
    (setq-local treesit-defun-tactic 'nested)
    (setq-local treesit-font-lock-settings
                (apply #'treesit-font-lock-rules
                       (lilypond-ts--font-lock-rules)))
    (setq-local treesit-font-lock-feature-list
                '((comment string escaped-word)
                  (keyword scheme expression object markup)
                  (number phrasing)))
    (setq-local treesit-font-lock-level 3)
    (setq-local treesit-simple-indent-rules lilypond-ts-indent-rules)
    ;; (setq-local treesit--indent-verbose t)
    ;; (setq-local treesit--font-lock-verbose t)
    (setq-local treesit-simple-imenu-settings lilypond-ts-imenu-rules)
    (add-hook 'lilypond-ts-post-eval-hook #'lilypond-ts--require-list-refresh)
    (treesit-major-mode-setup)
    (when (featurep 'geiser-lilypond-guile)
      (geiser-mode 1)
      (add-hook 'completion-at-point-functions
                #'lilypond-ts--property-capf nil t)
      (add-hook 'completion-at-point-functions
                #'lilypond-ts--symbol-capf nil t)
      (add-hook 'completion-at-point-functions
                #'lilypond-ts--escaped-word-capf nil t))
    (setq-local lisp-indent-function #'scheme-indent-function)
    (setq-local syntax-propertize-function
                #'lilypond-ts--propertize-syntax)))

(add-to-list 'auto-mode-alist '("\\.ly\\'" . lilypond-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . lilypond-ts-mode))

(provide 'lilypond-ts-mode)
;;; lilypond-ts-mode.el ends here
