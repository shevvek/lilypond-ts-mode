;; -*- lexical-binding: t -*-
(use-package pcre2el :ensure t)

(defvar ts-auto-query-files nil)
(defvar ts-auto-query-lang nil)
(defvar ts-auto-query-dir "auto-queries/")

(defun insert-query-specs (feature)
  ;; Move from end of last sexp to beginning of next sexp
  (forward-sexp)
  (when (thing-at-point 'sexp)
    (backward-sexp)
    (apply #'insert `(":language "
                      ,ts-auto-query-lang
                      "\n:feature "
                      ,feature
                      "\n("))))

(defun format-qualifier ()
  (let ((this-char (thing-at-point 'char)))
    (cond
     ((equal "." this-char)
      (delete-char 1)
      (insert ":anchor"))
     ((or (equal "?" this-char)
          (equal "*" this-char)
          (equal "+" this-char))
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
                             (prin1-to-string
                              (rxt-pcre-to-elisp (sexp-at-point))))))

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
    (insert-query-specs out-name)
    (while (not (eobp))
      (forward-sexp)
      (when (eolp)
        (insert ")")
        (insert-query-specs out-name)))
    (insert "))\n\n(provide '" out-name ")")
    (while (re-search-backward (rx (or "." "*" "?" "+")) nil t)
      (unless (or (syntax-ppss-context (syntax-ppss))
                  (> (length (thing-at-point 'sexp)) 1))
        (format-qualifier)))
    (goto-char (point-min))
    (insert "(defvar " out-name "\n'(\n")
    (while (search-forward "(#" nil t)
      (handle-query-predicate))
    (emacs-lisp-mode)
    (indent-region (point-min) (point-max))
    (write-file (concat ts-auto-query-dir out-name ".el"))
    (let (selector-list)
      (goto-char (point-max))
      (while (re-search-backward (rx "@" (1+ word) (0+ "." (1+ word))) nil t)
        (push (thing-at-point 'sexp) selector-list))
      selector-list)))

(defun ts-auto-parse-queries (ts-ly-loc)
  (let ((out-names (mapcar #'cdr ts-auto-query-files))
        (selectors (mapcan (lambda (qfile)
                             (translate-ts-query-file (concat ts-ly-loc (car qfile))
                                                      (cdr qfile)))
                           ts-auto-query-files)))
    (with-temp-buffer
      (emacs-lisp-mode)
      (mapc (lambda (oname)
              (insert "(require '"
                      oname
                      ")\n"))
            out-names)
      (insert "\n(defvar auto-ly-font-lock-rules\n"
              "(nconc "
              (string-join out-names "\n")
              "))\n")
      (mapc (lambda (selector)
              (insert "\n(defface "
                      selector
                      " '((t :inherit default))\n"
                      "\"Auto-generated face for treesit selector: "
                      selector
                      "\")"))
            (sort (seq-uniq selectors) #'string< ))
      (insert "\n\n(provide 'auto-ly-font-lock-rules)")
      (indent-region (point-min) (point-max))
      (write-file (concat ts-auto-query-dir "auto-ly-font-lock-rules.el")))))

(provide 'ts-auto-parse-queries)
