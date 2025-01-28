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

(add-to-list 'load-path
             (file-name-concat lilypond-ts-location "ts-auto-parse-queries"))
(require 'ts-auto-parse-queries)

(defvar lilypond-ts-use-auto-queries nil)

(defun lilypond-ts-install-auto-queries (&optional ts-ly-dir)
  (interactive)
  (let ((default-directory lilypond-ts-location)
        (grammar-loc (or ts-ly-dir
                         (read-directory-name
                          "Local location of tree-sitter-lilypond repo: "))))
    (unless ts-auto-query-lang
      (setq ts-auto-query-lang "lilypond"))
    (unless ts-auto-query-files
      (setq ts-auto-query-files
            '(("queries/highlights.scm" . "highlights")
              ("queries/highlights-builtins.scm" . "highlights-builtins")
              ("tree-sitter-lilypond-scheme/queries/highlights.scm"
               . "scheme-highlights")
              ("tree-sitter-lilypond-scheme/queries/highlights-builtins.scm"
               . "scheme-highlights-builtins")
              ("tree-sitter-lilypond-scheme/queries/highlights-lilypond-builtins.scm"
               . "scheme-highlights-lilypond-builtins"))))
    (ts-auto-parse-queries grammar-loc)))

(defun lilypond-ts--legacy-install ()
  (add-to-list 'treesit-language-source-alist
               `(lilypond . (,lilypond-ts-grammar-url)))
  (treesit-install-language-grammar 'lilypond))

(defun lilypond-ts--future-install ()
  (let ((src-dir (file-name-concat lilypond-ts-location "grammar-src")))
    ;; As of 30.0.92, treesit--git-clone-repo does not handle git submodules
    (treesit--git-clone-repo lilypond-ts-grammar-url
                             nil src-dir)
    (treesit--install-language-grammar-1 nil 'lilypond src-dir)
    (lilypond-ts-install-auto-queries (expand-file-name src-dir))))

(defun lilypond-ts-install ()
  (if (fboundp 'treesit--git-clone-repo)
      (lilypond-ts--future-install)
    (lilypond-ts--legacy-install)))

(unless (treesit-language-available-p 'lilypond)
  (lilypond-ts--legacy-install))

(when lilypond-ts-use-auto-queries
  (let ((auto-query-loc (file-name-concat lilypond-ts-location
                                          ts-auto-query-dir)))
    (unless (file-directory-p auto-query-loc)
      (lilypond-ts-install-auto-queries))
    (add-to-list 'load-path
                 (file-name-concat lilypond-ts-location ts-auto-query-dir))
    (require 'auto-ly-font-lock-rules)))

;;; Options

(defvar lilypond-ts--debug-msgs nil)

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

(defun lang-block-parent (node &rest _)
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
                      (treesit-node-type (lang-block-parent node))))
      ;; calculate-lisp-indent already takes initial indent into account
      column-0
      (lambda (node &rest _)
        (calculate-lisp-indent (treesit-node-start
                                (lang-block-parent node)))))

     ;; Base top level indentation
     ((parent-is "lilypond_program") column-0 0)
     ;; Fallback default
     (catch-all parent 0)
     )))

;;; Imenu

(defvar lilypond-ts-imenu-rules
  `(("Definitions" "assignment_lhs"
     ,(lambda (node)
        (equal "lilypond_program"
               (treesit-node-type
                (treesit-node-parent node))))
     treesit-node-text)
    ("ParserDefines" "escaped_word"
     ,(lambda (node)
        (equal "\\parserDefine"
               (treesit-node-text node)))
     ,(lambda (node)
        (treesit-node-text
         (treesit-node-next-sibling node))))
    ("Contexts" "named_context"
     ,(lambda (node)
        (= 4 (treesit-node-child-count node)))
     treesit-node-text)))

;;; Font lock

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
  nil)

(defvar lilypond-ts--grobs
  nil)

(defvar lilypond-ts--translators
  nil)

(defvar lilypond-ts--markup-functions
  nil)

(defvar lilypond-ts--post-events
  nil)

(defvar lilypond-ts--event-functions
  nil)

(defun lilypond-ts--fontify-scheme (node override start end &rest _)
  (when (featurep 'geiser-lilypond-guile)
    (let ((scheme-ranges (lilypond-ts--scheme-ranges (treesit-node-start node)
                                                     (treesit-node-end node))))
      (dolist (range-interval scheme-ranges)
        (apply #'geiser-syntax--fontify-syntax-region range-interval)))))

(defun lilypond-ts--init-keywords ()
  (setq lilypond-ts--contexts
        (mapcar #'symbol-name
                (geiser-eval--send/result '(:eval (ly:all-context-names)))))
  (setq lilypond-ts--translators
        (mapcar #'symbol-name
                (geiser-eval--send/result '(:eval (ly:all-translator-names)))))
  (setq lilypond-ts--grobs
        (mapcar #'symbol-name
                (geiser-eval--send/result '(:eval (ly:all-grob-names)))))
  (setq lilypond-ts--markup-functions
        (mapcar
         (lambda (sym)
           (string-trim-right (symbol-name sym) "-markup\\(-list\\)?"))
         (geiser-eval--send/result
          '(:eval (keywords-of-type (lambda (v)
                                      (or (markup-function? v)
                                          (markup-list-function? v))))))))
  (setq lilypond-ts--post-events
        (seq-remove (lambda (str)
                      (string-match-p (rx bol "#{"
                                          (* anything)
                                          "}#" eol)
                                      str))
                    (mapcar #'symbol-name
                            (geiser-eval--send/result
                             '(:eval (keywords-of-type ly:event?))))))
  (setq lilypond-ts--event-functions
        (mapcar #'symbol-name
                (geiser-eval--send/result
                 '(:eval (keywords-of-type ly:event-function?))))))

(defun lilypond-ts--maybe-init-keywords ()
  (when (and (null lilypond-ts--contexts)
             (null lilypond-ts--translators)
             (null lilypond-ts--grobs)
             (null lilypond-ts--markup-functions)
             (null lilypond-ts--post-events)
             (featurep 'geiser-lilypond-guile))
    (unless (seq-contains-p (geiser-repl--repl-list)
                            '(lilypond-guile))
      (geiser 'lilypond-guile))
    (unless geiser-impl--implementation
      (geiser-impl--set-buffer-implementation 'lilypond-guile))
    (unless (geiser-eval--send/result '(:eval :t))
      (message "lilypond-ts-mode wasn't able to connect to Geiser Lilypond-Guile
REPL to initialize word lists."))
    (lilypond-ts--init-keywords)))

(defun lilypond-ts--object-node-p (node)
  (seq-contains-p `(,@lilypond-ts--contexts
                    ,@lilypond-ts--grobs
                    ,@lilypond-ts--translators)
                  (treesit-node-text node)))

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
      (:match ,(eval `(rx bol (or ,@lilypond-ts--contexts
                                  ,@lilypond-ts--grobs)
                          eol))
              @font-lock-type-face))
     ((escaped_word) @font-lock-type-face
      (:match ,(eval `(rx bol "\\" (or ,@lilypond-ts--contexts)
                          eol))
              @font-lock-type-face)))

    :feature object
    :override prepend
    (((symbol) @bold
      (:match ,(eval `(rx bol (or ,@lilypond-ts--contexts)
                          eol))
              @bold))
     ((escaped_word) @bold
      (:match ,(eval `(rx bol "\\" (or ,@lilypond-ts--contexts)
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
                                       ,@lilypond-ts--markup-functions)
                          eol))
              @font-lock-function-call-face)))

    :feature expression
    :override t
    (((dynamic) @font-lock-builtin-face)
     (((escaped_word) @font-lock-builtin-face
       (:match  ,(eval `(rx bol (? "\\") ;; optional in order to match \^ and \-
                            (or ,@lilypond-ts--post-events
                                ,@lilypond-ts--event-functions)
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
      :anchor (property_expression))
     ((((escaped_word) @font-lock-keyword-face
        (:match ,(rx bol "\\" "=" eol) @font-lock-keyword-face))
       :anchor
       (unsigned_integer) @font-lock-number-face)))

    :feature phrasing
    :override prepend
    ((punctuation ["\\(" "\\)"]) @font-lock-variable-name-face @bold)
    ))

;;; Completion

(defsubst lilypond-ts--context-p (str)
  (seq-contains-p lilypond-ts--contexts str))

(defsubst lilypond-ts--grob-p (str)
  (eq :t (geiser-eval--send/result
          `(:eval (object-property (string->symbol ,str) 'is-grob?)))))

(defsubst lilypond-ts--grob-property-p (str)
  (eq :t (geiser-eval--send/result
          `(:eval (object-property (string->symbol ,str) 'backend-type?)))))

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
         (prop-ex-p (treesit-node-match-p parent-node "property_expression"))
         (func-node (treesit-search-forward this-node "escaped_word" t))
         (func-text (string-trim-left (treesit-node-text func-node t)
                                      "\\\\"))
         (left-sib (when prop-ex-p (treesit-node-prev-sibling
                                    (treesit-node-prev-sibling this-node))))
         (left-sib (if (treesit-node-match-p left-sib "property_expression")
                       (car (last (treesit-node-children left-sib)))
                     left-sib))
         (right-sib (when prop-ex-p (treesit-node-next-sibling
                                     (treesit-node-next-sibling this-node))))
         (left-text (treesit-node-text left-sib t))
         (right-text (treesit-node-text right-sib t))
         (left-ctx-p (when left-sib
                       (seq-contains-p lilypond-ts--contexts left-text)))
         (left-grob-p (when (and left-sib
                                 (not left-ctx-p))
                        (lilypond-ts--grob-p left-text)))
         (right-grob-p (when right-sib
                         (lilypond-ts--grob-p right-text)))
         (right-grob-prop-p (when (and right-sib
                                       (not right-grob-p))
                              (lilypond-ts--grob-property-p right-text)))
         (right-ctx-prop-p
          (when (and right-sib
                     (not right-grob-p)
                     (not right-grob-prop-p))
            (eq :t (geiser-eval--send/result
                    `(:eval (object-property (string->symbol ,right-text)
                                             'translation-type?)))))))
    (when lilypond-ts--debug-msgs
      (message "Debug lilypond-ts--property-completions: %s %s %s"
               func-text left-text right-text))
    (when (and (treesit-node-match-p this-node "symbol")
               (or prop-ex-p
                   (seq-contains-p lilypond-ts--context-property-functions
                                   func-text)
                   (seq-contains-p lilypond-ts--grob-property-functions
                                   func-text)))
      (append
       (when (and (not left-sib)
                  (or (not right-sib) right-grob-p right-ctx-prop-p))
         lilypond-ts--contexts)
       (when (and (not (seq-contains-p lilypond-ts--context-property-functions
                                       func-text))
                  (or (not left-sib) left-ctx-p)
                  (or (not right-sib) right-grob-prop-p))
         lilypond-ts--grobs)
       (when left-grob-p
         (geiser-eval--send/result
          `(:eval (ly:grob-property-completions ,left-text ,right-text))))
       (when (and (not (seq-contains-p lilypond-ts--grob-property-functions
                                       func-text))
                  (or (not left-sib) left-ctx-p))
         (geiser-eval--send/result '(:eval all-translation-properties)))))))

(defun lilypond-ts--property-capf (&optional predicate)
  (and-let* (((treesit-parser-list (current-buffer) 'lilypond))
             (this-node (treesit-node-at (point)))
             (this-node (if (treesit-node-match-p this-node "punctuation")
                            (treesit-node-at (1- (point)))
                          this-node))
             ((treesit-node-match-p this-node "symbol"))
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
     lilypond-ts--contexts
     lilypond-ts--markup-functions
     (geiser-eval--send/result `(:eval (keywords-of-type ly:music-word?
                                                         ,pfx))))))

(defun lilypond-ts--escaped-word-capf (&optional predicate)
  (and-let* (((treesit-parser-list (current-buffer) 'lilypond))
             (this-node (treesit-node-at (point)))
             ((treesit-node-match-p this-node "escaped_word"))
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

;;; Mode-init

(define-derived-mode lilypond-ts-mode prog-mode "Lilypond"
  (when (treesit-ready-p 'lilypond)
    (setq-local treesit-primary-parser (treesit-parser-create 'lilypond))
    (if lilypond-ts-use-auto-queries
        (progn
          (setq-local treesit-font-lock-feature-list auto-ly-font-lock-features)
          (setq-local treesit-font-lock-level 1)
          (setq-local treesit-font-lock-settings
                      (apply #'treesit-font-lock-rules
                             auto-ly-font-lock-rules)))
      (progn
        (lilypond-ts--maybe-init-keywords)
        (setq-local treesit-font-lock-settings
                    (apply #'treesit-font-lock-rules
                           (lilypond-ts--font-lock-rules)))
        (setq-local treesit-font-lock-feature-list
                    '((comment string escaped-word)
                      (keyword scheme expression object markup)
                      (number phrasing)))
        (setq-local treesit-font-lock-level 3)))
    (setq-local treesit-simple-indent-rules lilypond-ts-indent-rules)
    ;; (setq-local treesit--indent-verbose t)
    ;; (setq-local treesit--font-lock-verbose t)
    (setq-local treesit-simple-imenu-settings lilypond-ts-imenu-rules)
    (treesit-major-mode-setup)
    (when (featurep 'geiser-lilypond-guile)
      (geiser-mode 1)
      (add-hook 'completion-at-point-functions
                #'lilypond-ts--property-capf nil t)
      (add-hook 'completion-at-point-functions
                #'lilypond-ts--escaped-word-capf nil t))
    (setq-local lisp-indent-function #'scheme-indent-function)
    (setq-local syntax-propertize-function
                #'lilypond-ts--propertize-syntax)))

(add-to-list 'auto-mode-alist '("\\.ly\\'" . lilypond-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . lilypond-ts-mode))

(provide 'lilypond-ts-mode)
;;; lilypond-ts-mode.el ends here
