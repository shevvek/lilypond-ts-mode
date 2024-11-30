(require 'treesit)

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

    ;; (
    ;;  (string
    ;;   "\"" @string.delimiter.left
    ;;   [
    ;;    (string_fragment)?
    ;;    (escape_sequence)? @string.escape
    ;;    ]
    ;;   "\"" @string.delimiter.right
    ;;   )
    ;;  ) @string

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
    (setq-local treesit-font-lock-feature-list
                '((comments punctuation)
                  (functions variables)
                  (dynamics)
                  (lexer numbers)
                  (operators grouping)))
    (setq-local treesit-font-lock-level 5)
    (setq-local treesit-font-lock-settings
                (apply #'treesit-font-lock-rules lilypond-ts-font-lock-rules))
    (treesit-parser-create 'lilypond)
    (treesit-major-mode-setup)))

(add-to-list 'auto-mode-alist '("\\.ly\\'" . lilypond-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . lilypond-ts-mode))

(provide 'lilypond-ts-mode)
;;; lilypond-ts-mode.el ends here
