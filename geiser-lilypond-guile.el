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

(defun ly-guile--eval-result (code)
  (string-trim-left
   (geiser-eval--retort-output (geiser-eval--send/wait code))
   "\\$[[:digit:]]+ = "))

(defun ly-guile--init-keyword (code)
  (split-string (geiser-eval--retort-output
                 (geiser-eval--send/wait code))
                "[^[:alpha:]-_]+" t))

(defvar ly-guile-keyword-cmds-alist
  '((music-functions . "(all-keywords-of-type ly:music-function?)")
    (markup-functions . "(all-keywords-of-type markup-function?)")
    (contexts . "(map car (ly:output-find-context-def $defaultlayout))")
    (translators . "(map ly:translator-name (ly:get-all-translators))")
    (context-props . "all-translation-properties")
    (grob-props . "all-backend-properties")
    (music-props . "all-music-properties")
    (music-types . "(map car music-descriptions)")
    (grobs . "(map car all-grob-descriptions)")))

(defvar ly-guile-keywords-alist nil)

(defun ly-guile-init-keywords ()
  (dolist (kv ly-guile-keyword-cmds-alist)
    (add-to-list 'ly-guile-keywords-alist
                 (cons (car kv)
                       (ly-guile--init-keyword (cdr kv))))))

(defun ly-guile-repl-startup (address)
  (geiser-guile--startup (or address t))
  (geiser-eval--send/wait "(load-from-path \"ly-guile-autodoc.scm\")"))

(define-geiser-implementation (lilypond-guile guile)
                              (binary ly-guile-bin)
                              (arglist ly-guile-args)
                              (repl-startup ly-guile-repl-startup)
                              (version-command ly-guile-version))
(geiser-implementation-extension 'lilypond-guile "scm")
(geiser-activate-implementation 'lilypond-guile)

(provide 'geiser-lilypond-guile)
;;; geiser-lilypond-guile.el ends here
