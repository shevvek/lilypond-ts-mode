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

(defvar lilypond-ts-indent-offset 2)
(defvar lilypond-ts-indent-rules
  `((lilypond
     ;; Align braces and brackets with the surrounding scope
     ((node-is "{") parent-bol 0)
     ((node-is "<<") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is ">>") parent-bol 0)
     ;; Indent inside curly braces {}
     ((parent-is "expression_block") parent-bol ,lilypond-ts-indent-offset)
     ;; Indent inside double angle brackets << >>
     ((parent-is "parallel_music") parent-bol ,lilypond-ts-indent-offset)
     ;; Default rule: no additional indentation
     (no-node parent 0))))

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

(define-derived-mode lilypond-ts-mode prog-mode "Lilypond"
  (when (treesit-ready-p 'lilypond)
    (when lilypond-ts-use-auto-queries
      (setq-local treesit-font-lock-feature-list auto-ly-font-lock-features)
      (setq-local treesit-font-lock-level 1)
      (setq-local treesit-font-lock-settings
                  (apply #'treesit-font-lock-rules auto-ly-font-lock-rules)))
    (setq-local treesit-simple-indent-rules lilypond-ts-indent-rules)
    (setq-local treesit--indent-verbose t)
    (setq-local treesit-simple-imenu-settings lilypond-ts-imenu-rules)
    (treesit-parser-create 'lilypond)
    (treesit-major-mode-setup)))

(add-to-list 'auto-mode-alist '("\\.ly\\'" . lilypond-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . lilypond-ts-mode))

(provide 'lilypond-ts-mode)
;;; lilypond-ts-mode.el ends here
