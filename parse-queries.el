;; -*- lexical-binding: t -*-
(use-package pcre2el :ensure t)

(defvar query-specs
  '(":language lilypond\n"
    ":feature auto\n"
    "("))

(defun insert-query-specs ()
  ;; Move from end of last sexp to beginning of next sexp
  (forward-sexp)
  (when (thing-at-point 'sexp)
    (backward-sexp)
    (apply #'insert query-specs)))

(defun format-qualifier ()
  (let ((this-sym (symbol-name (symbol-at-point))))
    (cond
     ((equal "." this-sym)
      (delete-char 1)
      (insert ":anchor"))
     ((string-match-p (rx bol (or "?" "*" "+") eol) this-sym)
      (insert ":"))
     )))

  (defun format-pred ()
    ;; Assume point is just after #
    (delete-char -1)
    (insert ":")
    (forward-sexp)
    (delete-char -1))

(defun translate-match-query ()
  ;; Assume the point is just after :match
  (forward-sexp)
  (transpose-sexps 1)
  (backward-sexp 2)
  (mark-sexp 1)
  ;; The region should now be exactly the regex string
  (replace-region-contents (point) (mark)
                           (lambda ()
                             (concat
                              ",(rx "
                              (prin1-to-string
                               (rxt-pcre-to-rx
                                (sexp-at-point)))
                              ")"))))

(defun handle-query-predicate ()
  (let ((pred (thing-at-point 'word)))
    (cond
     ((equal "match" pred)
      (format-pred)
      (translate-match-query))
     ((equal "equal" pred)
      (format-pred))
     ((equal "pred" pred)
      (format-pred)
      (display-warning 'ts-query-translate
                       "Custom predicates must be manually translated"))
     (t ;; else
      (backward-up-list)
      (kill-sexp)))))

(defun translate-ts-query-file (scm-file out-name)
  (with-temp-buffer
    (insert-file-contents scm-file)
    (scheme-mode)
    (goto-char (point-min))
    (insert-query-specs)
    (while (not (eobp))
      (forward-sexp)
      (when (eolp)
        (insert ")")
        (insert-query-specs)))
    (insert "))\n\n(provide '" out-name ")")
    (while (not (bobp))
      (format-qualifier)
      (forward-symbol -1))
    (insert "(defvar " out-name " `(\n")
    (while (search-forward "(#" nil t)
      (handle-query-predicate))
    (write-file (concat out-name ".el"))))

(defvar query-files nil)
(setq query-files
      '(("queries/highlights.scm" . "highlights")
        ("queries/highlights-builtins.scm" . "highlights-builtins")
        ("tree-sitter-lilypond-scheme/queries/highlights.scm" . "scheme-highlights")
        ("tree-sitter-lilypond-scheme/queries/highlights-builtins.scm" . "scheme-highlights-builtins")
        ("tree-sitter-lilypond-scheme/queries/highlights-lilypond-builtins.scm" . "scheme-highlights-lilypond-builtins")))

(defun translate-ly-queries (ts-ly-loc)
  (mapc (lambda (qfile)
          (translate-ts-query-file (concat ts-ly-loc (car qfile))
                                   (cdr qfile)))
        query-files))

(translate-ly-queries "D:/tree-sitter-lilypond/")
