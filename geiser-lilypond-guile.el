;;; geiser-lilypond-guile.el --- Lilypond Geiser REPL -*- lexical-binding: t -*-

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

(use-package geiser-guile)

(defvar ly-guile-bin "lilypond")
(defvar ly-guile-args '("scheme-sandbox"))
(defvar ly-guile-version "3.0.10")

(defvar ly-guile-dir
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'geiser-guile-load-path (file-name-concat ly-guile-dir "scm/"))

(setq geiser-guile-extra-keywords
      '("define-syntax-function"
        "define-syntax-public"
        "define-syntax-rule"
        "define-music-function"
        "define-scheme-function"
        "define-void-function"
        "define-event-function"
        "define-method"
        "define-markup-command"
        "define-markup-list-command"
        "define-session"
        "define-session-public"
        "make-translator"
        "make-performer"
        "make-engraver"
        "make-translator-internal"
        "make-translator-component"
        "make-relative"
        "_i"
        "G_"
        "*parser*"
        "*location*"))

(defun ly-guile-repl-startup (address)
  (geiser-guile--startup (or address t))
  (geiser-eval--send/wait '(:eval (use-modules (geiser-lilypond)))))

(defun ly-guile--symbol-begin (module)
  (if (and (featurep 'treesit)
           (treesit-parser-list (current-buffer))
           (not module))
      (or (car (bounds-of-thing-at-point 'symbol))
          (save-excursion (beginning-of-line) (point)))
    (geiser-guile--symbol-begin module)))

(define-geiser-implementation (lilypond-guile guile)
                              (binary ly-guile-bin)
                              (arglist ly-guile-args)
                              (repl-startup ly-guile-repl-startup)
                              (find-symbol-begin ly-guile--symbol-begin)
                              (version-command ly-guile-version))
(geiser-implementation-extension 'lilypond-guile "scm")
(geiser-activate-implementation 'lilypond-guile)

(provide 'geiser-lilypond-guile)
;;; geiser-lilypond-guile.el ends here
