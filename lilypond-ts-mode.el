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
    "accepts" "addlyrics" "alias"  "alternative" "book" "bookpart" "change"
    "chordmode" "chords" "consists" "context" "default" "defaultchild" "denies"
    "description" "drummode" "drums" "etc" "figuremode" "figures" "header"
    "layout" "lyricmode" "lyrics" "lyricsto" "markup" "markuplist" "midi" "name"
    "new" "notemode" "override" "paper" "remove" "repeat" "rest" "revert"
    "score" "sequential" "set" "simultaneous" "tempo" "type" "unset"))

(defvar lilypond-ts--contexts
  nil)

(defvar lilypond-ts--grobs
  nil)

(defvar lilypond-ts--translators
  nil)

(defvar lilypond-ts--markup-functions
  nil)

(defvar lilypond-ts--priority-kwds
  '("absolute" "acciaccatura" "after" "afterGrace" "alterBroken" "appendToTag"
    "applyContext" "applyMusic" "applyOutput" "appoggiatura" "autoChange"
    "cadenzaOff" "cadenzaOn" "compoundMeter" "contextPropertyCheck" "cueDuring"
    "cueDuringWithClef" "fixed" "grace" "keepWithTag" "language"
    "languageRestore" "languageSaveAndChange" "markupMap" "once" "ottava"
    "overrideProperty" "parallelMusic" "partCombine" "partial"
    "popContextProperty" "propertyOverride" "propertyRevert" "propertySet"
    "propertyTweak" "propertyUnset" "pushContextProperty" "pushToTag"
    "quoteDuring" "relative" "removeWithTag" "scaleDurations" "settingsFrom"
    "single" "slashedGrace" "stopStaff" "tag" "tagGroup" "temporary" "time"
    "times" "transpose" "transposedCueDuring" "transposition" "tuplet" "tweak"
    "undo" "unfoldRepeats" "unfolded" "void" "volta"))

(defun lilypond-ts--fontify-scheme (node override start end &rest _)
  (let ((scheme-ranges (lilypond-ts--scheme-ranges (treesit-node-start node)
                                                   (treesit-node-end node))))
    (dolist (range-interval scheme-ranges)
      (apply #'geiser-syntax--fontify-syntax-region range-interval))))

(defun lilypond-ts--fontify-escaped-word (node override start end &rest _)
  (let ((this-word (string-trim-left (treesit-node-text node) "\\\\"))
        (node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (cond
     ((seq-contains-p '("include" "maininput" "version") this-word)
      (message "Got PP at %d" node-start)
      (treesit-fontify-with-override node-start node-end
                                     'font-lock-preprocessor-face
                                     override))
     ((seq-contains-p lilypond-ts--lexer-keywords this-word)
      (message "Got lex at %d" node-start)
      (treesit-fontify-with-override node-start node-end
                                     'font-lock-builtin-face
                                     override))
     ((seq-contains-p lilypond-ts--priority-kwds this-word)
      (message "Got priority at %d" node-start)
      (treesit-fontify-with-override node-start node-end
                                     'font-lock-keyword-face
                                     override))
     ((seq-contains-p '("breve" "longa" "maxima") this-word)
      (message "Got duration at %d" node-start)
      (treesit-fontify-with-override node-start node-end
                                     'font-lock-variable-use-face
                                     override))
     ;; ((not
     ;;   (string-equal "#t"
     ;;                 (ly-guile--eval-result
     ;;                  `(and (module-bound? (current-module) ',this-word)
     ;;                        (ly:music? ,this-word)
     ;;                        (music-is-of-type? ,this-word 'script-event)))))
     ;;  (message "Got script at %d" node-start)
     ;;  (treesit-fontify-with-override node-start node-end
     ;;                                 'font-lock-builtin-face
     ;;                                 override))
     ;; ((string-equal "#t"
     ;;                (ly-guile--eval-result
     ;;                 `(and (module-bound? (current-module) ',this-word)
     ;;                       (or (markup? ,this-word)
     ;;                           (markup-list? ,this-word))
     ;;                       (not (null? ,this-word)))))
     ;;  (message "Got markup at %d" node-start)
     ;;  (treesit-fontify-with-override node-start node-end
     ;;                                 'font-lock-variable-name-face
     ;;                                 override))
     ;; ((not
     ;;   (string-equal "#f"
     ;;                 (ly-guile--eval-result
     ;;                  `(or (lookup-markup-command ',this-word)
     ;;                       (lookup-markup-list-command ',this-word)))))
     ;;  (message "Got markup at %d" node-start)
     ;;  (treesit-fontify-with-override node-start node-end
     ;;                                 'font-lock-variable-use-face
     ;;                                 override))
     (t (treesit-fontify-with-override node-start node-end
                                       'font-lock-function-call-face
                                       override)))))

(defun lilypond-ts--init-keywords ()
  (setq-default lilypond-ts--contexts
                (ly-guile--init-keyword
                 "(map car (ly:output-find-context-def $defaultlayout))"))
  (setq-default lilypond-ts--translators
                (ly-guile--init-keyword
                 "(map ly:translator-name (ly:get-all-translators))"))
  (setq-default lilypond-ts--grobs
                (ly-guile--init-keyword "(map car all-grob-descriptions)")))

(defvar lilypond-ts--font-lock-rules nil)

(setq-default lilypond-ts--font-lock-rules
              `(
                :default-language lilypond

                :feature comment
                ((comment) @font-lock-comment-face)

                :feature assignment
                ((assignment_lhs) @font-lock-function-name-face)

                :feature escaped-word
                ((escaped_word) @lilypond-ts--fontify-escaped-word)

                :feature string
                (((string) @font-lock-string-face)
                 ((string (escape_sequence) @font-lock-escape-face)))

                :feature scheme
                ((embedded_scheme_text) @lilypond-ts--fontify-scheme)

                ;; :feature object
                ;; ([,@lilypond-ts--contexts
                ;;   ,@lilypond-ts--grobs
                ;;   ,@lilypond-ts--translators] 'font-lock-type-face)

                ;; :feature markup
                ;; ([,@lilypond-ts--markup-functions] 'font-lock-variable-use-face)

                ;; :feature number
                ;; (([(fraction)
                ;;    (decimal_number)] @font-lock-variable-use-face)
                ;;  ((unsigned_integer) @font-lock-variable-use-face)
                ;;  ;; (("*" :anchor
                ;;  ;;   [(fraction)
                ;;  ;;    (decimal_number)
                ;;  ;;    (unsigned_integer)]) @font-lock-variable-use-face)
                ;;  )
                :feature dynamic
                ((dynamic) @font-lock-keyword-face)

                ;; :feature articulation
                ;; ((["-" "_" "^"] @font-lock-builtin-face
                ;;   :anchor (escaped_word))
                ;;  ((["-" "_" "^"]
                ;;    :anchor
                ;;    ["!" "." "-" "^" "_" ">" "+"]) @font-lock-builtin-face)
                ;;  ((":" :anchor (unsigned_integer)) @font-lock-builtin-face))

                ;; :feature priority-kwd
                ;; (("\\" :anchor
                ;;   [,@lilypond-ts--priority-kwds]) @font-lock-keyword-face)


                ))

;;; Mode-init

(define-derived-mode lilypond-ts-mode prog-mode "Lilypond"
  (when (treesit-ready-p 'lilypond)
    (treesit-parser-create 'lilypond)
    (when lilypond-ts-use-auto-queries
      (setq-local treesit-font-lock-feature-list auto-ly-font-lock-features)
      (setq-local treesit-font-lock-level 1)
      (setq-local treesit-font-lock-settings
                  (apply #'treesit-font-lock-rules auto-ly-font-lock-rules)))
    (setq-local treesit-font-lock-settings
                (apply #'treesit-font-lock-rules
                       lilypond-ts--font-lock-rules))
    (setq-local treesit-font-lock-feature-list
                '((comment string scheme)
                  (dynamic assignment)
                  (escaped-word)))
    (setq-local treesit-font-lock-level 3)
    (setq-local treesit-simple-indent-rules lilypond-ts-indent-rules)
    (setq-local treesit--indent-verbose t)
    (setq-local treesit--font-lock-verbose t)
    (setq-local treesit-simple-imenu-settings lilypond-ts-imenu-rules)
    (treesit-major-mode-setup)
    (when (featurep 'geiser-lilypond-guile)
      (geiser-mode 1)
      (lilypond-ts--init-keywords))
    (setq-local lisp-indent-function #'scheme-indent-function)
    (setq-local syntax-propertize-function
                #'lilypond-ts--propertize-syntax)))

(add-to-list 'auto-mode-alist '("\\.ly\\'" . lilypond-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . lilypond-ts-mode))

(provide 'lilypond-ts-mode)
;;; lilypond-ts-mode.el ends here
