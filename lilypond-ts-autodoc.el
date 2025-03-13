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

;; Geiser autodoc mode wraps Emacs eldoc mode in order to repurpose it for use
;; with Scheme. We need to redefine autodoc-at-point to handle LilyPond syntax,
;; but we still want to use both the underlying Geiser library for querying the
;; REPL and caching function signatures, as well as the existing autodoc within
;; Scheme blocks. We also still want to use the top layer that defines a minor
;; mode wrapping Eldoc. The use of advice therefore seems unavoidable.

;;; Code:

(require 'geiser)
(require 'geiser-guile)
(require 'lilypond-ts-base)
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

;; The alternative here is literally copying Geiser's top layer of autodoc code.
(advice-add #'geiser-autodoc--autodoc-at-point
            :override #'lilypond-ts--autodoc-at-point)

(provide 'lilypond-ts-autodoc)
;;; lilypond-ts-autodoc.el ends here
