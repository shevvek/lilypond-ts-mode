;;; lilypond-ts-autodoc.el --- Autodoc  -*- lexical-binding: t -*-

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

;; Eldoc for LilyPond

;;; Code:

(require 'geiser)
(require 'geiser-guile)
(require 'lilypond-ts-syntax)
(require 'lilypond-ts-repl)

(defun lilypond-ts-autodoc--format-arg (arg)
  (concat
   (when (cddr arg) "[")
   (propertize (geiser-syntax--display (car arg)) 'face 'italic)
   " "
   (propertize (geiser-syntax--display (cadr arg))
               'face 'geiser-font-lock-autodoc-current-arg)
   (when (cddr arg)
     (format " = %s]" (geiser-syntax--display (caddr arg))))))

(defun lilypond-ts--autodoc-str (signature)
  (let ((type-p (assoc "type" signature)))
    (when type-p
      (let ((proc (car signature))
            (args (geiser-autodoc--sanitize-args
                   (cdr (assoc "args" signature))))
            (type (cdr type-p)))
        (format "%s: %s => %s"
                (geiser-autodoc--id-name proc nil)
                (mapconcat #'lilypond-ts-autodoc--format-arg args " ")
                (propertize (geiser-syntax--display type)
                            'face 'italic))))))

(defun lilypond-ts--capf-autodoc (id)
  "Return documentation string for ID"
  (when-let* ((name (string-trim-left id "\\\\"))
              (sig (car (geiser-autodoc--get-signatures (list name) nil)))
              ((cadr sig)))
    (or (lilypond-ts--autodoc-str sig)
        (geiser-autodoc--str* sig))))

(defun lilypond-ts--music-autodoc-at-point ()
  (cl-loop with node0 = (treesit-node-at (point))
           for node = (if (treesit-node-match-p node0 "escaped_word")
                          node0
                        (treesit-search-forward node0 "escaped_word" t))
           then (treesit-search-forward node "escaped_word" t)
           always node
           for name = (string-trim-left (treesit-node-text node t) "\\\\")
           ;; passing the callback through leads to output via geiser's format
           ;; this seems hard to patch since update-signatures duplicates path
           ;; construction functionality. so we do without async update
           ;; probably not that important in practice for lilypond
           for sig = (car (geiser-autodoc--get-signatures (list name) nil))
           until (cadr sig)
           finally return
           (when (or (treesit-node-eq node0 node)
                     (< (point)
                        (let ((arity (length (cdr (assoc "args" sig)))))
                          (treesit-node-end
                           (treesit-node-at
                            (treesit-navigate-thing (treesit-node-end node)
                                                    arity 'beg 'sexp))))))
             (lilypond-ts--autodoc-str sig))))

(defun lilypond-ts--autodoc-at-point (&optional callback)
  (if (or (not (treesit-parser-list (current-buffer) 'lilypond))
          (lilypond-ts--scheme-at-p))
      (geiser-autodoc--autodoc (geiser-syntax--scan-sexps) callback)
    (lilypond-ts--music-autodoc-at-point)))

;;; Code below is more or less verbatim from geiser-autodoc 0.31.1

(defun lilypond-ts--eldoc-function (&optional callback)
  (ignore-errors
    (when (not (geiser-autodoc--inhibit))
      (lilypond-ts--autodoc-at-point (or callback 'eldoc-message)))))

(defun lilypond-ts-autodoc-show ()
  "Show the signature or value of the symbol at point in the echo area."
  (interactive)
  (message (lilypond-ts--autodoc-at-point nil)))

(define-minor-mode lilypond-ts-autodoc-mode
  "Autodoc minor mode for LilyPond.

Note that `lilypond-ts-autodoc-mode' is built on top of `geiser-autodoc' and
respects `geiser-autodoc--inhibit-function'."
  :init-value nil
  :lighter "/A"
  (if lilypond-ts-autodoc-mode
      (add-hook 'eldoc-documentation-functions
                #'lilypond-ts--eldoc-function nil t)
    (remove-hook 'eldoc-documentation-functions
                 #'lilypond-ts--eldoc-function t))
  (eldoc-mode (if lilypond-ts-autodoc-mode 1 -1))
  (setq-local eldoc-minor-mode-string nil))

(provide 'lilypond-ts-autodoc)
;;; lilypond-ts-autodoc.el ends here
