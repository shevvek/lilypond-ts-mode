(require 'treesit)
(require 'ts-auto-parse-queries)

(defvar lilypond-ts-grammar-url
  "https://github.com/nwhetsell/tree-sitter-lilypond/")

(defvar lilypond-ts-location
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar lilypond-ts-use-auto-queries t)

(defun lilypond-ts-install-auto-queries (&optional ts-ly-dir)
  (interactive)
  (unless ts-ly-dir
    (setq-local ts-ly-dir (read-directory-name
                           "Local location of tree-sitter-lilypond repo: ")))
  (let ((default-directory lilypond-ts-location))
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
    (ts-auto-parse-queries ts-ly-dir)))

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

(defvar lilypond-ts-font-lock-rules
  `(;; tree-sitter-lilypond/queries/highlights.scm
    :language lilypond
    :feature comments
    :override t
    ((comment) @font-lock-comment-face)

    :language lilypond
    :feature punctuation
    :override nil
    ((punctuation) @font-lock-punctuation-face)

    :language lilypond
    :feature operators
    :override t
    ((
      (assignment_lhs)
      :anchor
      (
       (punctuation) @font-lock-operator-face
       (:match "\\`=\\'" @font-lock-operator-face)
       )
      ))

    :language lilypond
    :feature operators
    :override t
    ((named_context
      (symbol)
      :anchor
      (
       (punctuation) @font-lock-operator-face
       (:match "\\`=\\'" @font-lock-operator-face )
       )
      :anchor
      [(symbol) (string)]
      ))

    :language lilypond
    :feature grouping
    :override t
    ((chord
      :anchor
      "<" @font-lock-bracket-face
      ">" @font-lock-bracket-face
      :anchor
      ))

    :language lilypond
    :feature variables
    :override nil
    ((
      (escaped_word) @font-lock-variable-use-face
                                        ;(#not-match? @font-lock-variable-use-face \"^\\\\(?:include|maininput|version)$\") ; This is needed for Panic Nova
      ))

    :language lilypond
    :feature lexer
    :override t
    ((
      (escaped_word) @font-lock-type-face
      (:match ,(rx (seq bol "\\" (or "include" "maininput" "version") eol))
              @font-lock-type-face) ; These are handled directly by LilyPond’s lexer.
      ))

    :language lilypond
    :feature numbers
    :override t
    ((
      (escaped_word) @font-lock-keyword-face
      (:match ,(rx (seq bol "\\" (or "breve" "longa" "maxima") eol))
              @font-lock-keyword-face)
      ))

    ;; String bend rule
    ;; :language lilypond
    ;; :feature functions
    ;;                                     ;   :override t
    ;; ((
    ;;   (escaped_word) @font-lock-function-call-face
    ;;   (:match "^\\\\\\^$" @font-lock-function-call-face)
    ;;   ))

    ;; (quoted_identifier
    ;;  "\"" @bracket
    ;;  )

    ;; (
    ;;  (symbol) @keyword
    ;;  (#match? @keyword "^q$")
    ;;  )

    :language lilypond
    :feature numbers
                                        ; :override t
    ([
      (fraction)
      (decimal_number)
      (unsigned_integer)
      ] @font-lock-keyword-face)

    :language lilypond
    :feature dynamics
    :override t
    ((dynamic) @font-lock-constant-face)

    ;; (instrument_string_number) @identifier.core.function

    :language lilypond
    :feature strings
    ;; :override t
    ((
      (string
       "\"" ;;@string.delimiter.left
       [
        (string_fragment):?
        (escape_sequence):? @font-lock-escape-face
        ]
       "\"" ;;@string.delimiter.right
       )
      ) @font-lock-string-face)

    :language lilypond
    :feature grouping
                                        ; :override t
    ([
      "{" "}"
      "<<" (parallel_music_separator) ">>"
      "#{" "#}"
      ] @font-lock-bracket-face)

    ;; (chord
    ;;  ">>" @invalid
    ;;  )

    :language lilypond
    :feature lexer
                                        ;  :override t
    ((embedded_scheme_prefix) @font-lock-preprocessor-face)
    ))

(define-derived-mode lilypond-ts-mode prog-mode "Lilypond"
  (when (treesit-ready-p 'lilypond)
    ;; (setq-local treesit-font-lock-feature-list
    ;;             '((comments strings punctuation)
    ;;               (functions variables)
    ;;               (dynamics)
    ;;               (lexer numbers)
    ;;               (operators grouping)))
    (when lilypond-ts-use-auto-queries
      (setq-local treesit-font-lock-feature-list auto-ly-font-lock-features)
      (setq-local treesit-font-lock-level 1)
      (setq-local treesit-font-lock-settings
                  (apply #'treesit-font-lock-rules auto-ly-font-lock-rules)))
    (treesit-parser-create 'lilypond)
    (treesit-major-mode-setup)))

(add-to-list 'auto-mode-alist '("\\.ly\\'" . lilypond-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . lilypond-ts-mode))

(provide 'lilypond-ts-mode)
;;; lilypond-ts-mode.el ends here
