(require 'treesit)

(defvar lilypond-ts-font-lock-rules
  '(
    :language lilypond
    :feature comment
    :override t
    "(comment) @font-lock-comment-face"))

(define-derived-mode lilypond-ts-mode prog-mode "Lilypond"
  (when (treesit-ready-p 'lilypond)
    (setq-local treesit-font-lock-feature-list
                '((comment)))
    (setq-local treesit-font-lock-settings
                (apply #'treesit-font-lock-rules lilypond-ts-font-lock-rules))
    (treesit-parser-create 'lilypond)
    (treesit-major-mode-setup)))

(add-to-list 'auto-mode-alist '("\\.ly\\'" . lilypond-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . lilypond-ts-mode))

(provide 'lilypond-ts-mode)
;;; lilypond-ts-mode.el ends here
