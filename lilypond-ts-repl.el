;;; lilypond-ts-repl.el --- LilyPond Geiser REPL  -*- lexical-binding: t -*-

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

;; Typically, the interactive environment would be provided by a separate
;; package than the major mode. In this case, that would serve two purposes:
;;
;; 1. Provide a LilyPond Geiser implementation for use outside of
;; lilypond-ts-mode, such as in Scheme files.
;;
;; 2. Allow for use of lilypond-ts-mode without the extra weight of the REPL.
;;
;; The latter was already not supported. The former no longer seems compelling,
;; because scheme-mode is unable to handle LilyPond code embedded using #{ #}.
;; Since it is now planned for the lilypond-ts-mode package to provide a major
;; mode for LilyPond Scheme files, there is little application for a LilyPond
;; Geiser package to ever be provided separately from lilypond-ts-mode.
;;
;; This decision allows the REPL to more easily make use of new lilypond-ts-run
;; features, such as automatic version selection and argument sets, as it is no
;; longer a concern to have mutual dependencies between lilypond-ts-mode and the
;; Geiser implementation.
;;
;; In the future, a lightweight version of lilypond-ts-mode could be offered
;; that excludes Geiser dependencies, but it is probably only worth the effort
;; if there is demand from users. Since right now there aren't really any users,
;; the plan for building toward an initial beta release is that the Geiser REPL
;; will be an integral part of lilypond-ts-mode.

;;; Code:

(require 'geiser)
(require 'geiser-guile)
(require 'lilypond-ts-base)
(require 'lilypond-ts-run)

(add-to-list 'geiser-guile-load-path
             (file-name-concat lilypond-ts-location "scm/"))

(defun lilypond-ts--repl-startup (address)
  (geiser-guile--startup (or address t))
  (geiser-eval--send/wait '(:eval (use-modules (geiser-lilypond)))))

(defvar lilypond-ts--repl-lilypond nil)
(defvar lilypond-ts--repl-arglist nil)
(defvar lilypond-ts--repl-environment nil)

(defun lilypond-ts--get-repl-config ()
  (let* ((cmd-plist (lilypond-ts--flatten-cmd 'repl))
         (args (apply #'lilypond-ts--format-lily-args cmd-plist))
         (ver (plist-get cmd-plist :version))
         (lily-ver (lilypond-ts--closest-compatible-lily ver)))
    (if lily-ver
        (setq-local lilypond-ts--repl-lilypond lily-ver
                    lilypond-ts--repl-arglist (nconc args '("scheme-sandbox"))
                    lilypond-ts--repl-environment (plist-get cmd-plist :env))
      (warn "REPL couldn't find a LilyPond install compatible with version %s."
            ver))))

(defun lilypond-ts--ensure-repl ()
  (unless geiser-impl--implementation
    (geiser-impl--set-buffer-implementation 'lilypond-guile))
  (unless (and (geiser-repl--ensure-repl-buffer)
               (geiser-repl--live-p))
    (lilypond-ts--get-repl-config)
    (let ((process-environment (append lilypond-ts--repl-environment
                                       (copy-sequence process-environment))))
      (save-excursion
        (geiser-repl--start-repl 'lilypond-guile nil))))
  (geiser-repl--live-p))

(defsubst lilypond-ts--repl-bin (&rest _)
  (cadr lilypond-ts--repl-lilypond))

(defsubst lilypond-ts--repl-guile-version (&rest _)
  (plist-get lilypond-ts--repl-lilypond :guile-version))

(define-geiser-implementation (lilypond-guile guile)
                              (binary lilypond-ts--repl-bin)
                              (arglist lilypond-ts--repl-arglist)
                              (repl-startup lilypond-ts--repl-startup)
                              (version-command lilypond-ts--repl-guile-version))
(geiser-implementation-extension 'lilypond-guile "scm")
(geiser-activate-implementation 'lilypond-guile)

;;; Interactive evaluation

(defun lilypond-ts-eval-region (start end)
  "Async eval the region within the current Geiser LilyPond REPL."
  (interactive "r")
  (let* ((start-node (treesit-node-at start))
         (end-node (treesit-node-at end))
         (start-lang-block (lilypond-ts--lang-block-parent start-node))
         (end-lang-block (lilypond-ts--lang-block-parent end-node))
         (one-lang-parent-p (treesit-node-eq start-lang-block end-lang-block))
         (start (treesit-node-start start-node))
         (end (treesit-node-end end-node)))
    (cond
     ((not one-lang-parent-p)
      (message "lilypond-ts-eval-region error: start and end node do not belong to the same language block."))
     ((treesit-node-match-p start-lang-block "embedded_scheme_text")
      (geiser-eval-region start end)
      (run-hooks 'lilypond-ts-post-eval-hook))
     (t (geiser-eval--send
         `(:eval (ly:parser-parse-string
                  (ly:parser-clone)
                  ,(buffer-substring-no-properties start end)))
         (lambda (s)
           (message "%s" (geiser-eval--retort-result-str s nil))))
        (run-hooks 'lilypond-ts-post-eval-hook)))))

(defun lilypond-ts-eval-buffer (&optional buffer)
  "Async eval a LilyPond buffer within the current Geiser LilyPond REPL."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (fname (expand-file-name (buffer-file-name buf))))
    (with-current-buffer buf
      (if (file-exists-p fname)
          (geiser-eval--send
           `(:eval (ly:parser-parse-string
                    (ly:parser-clone)
                    ,(format "\\include \"%s\"" fname)))
           (lambda (s)
             (run-hooks 'lilypond-ts-post-eval-hook)
             (message "%s" s)))
        (lilypond-ts-eval-region (point-min) (point-max))))))

(provide 'geiser-lilypond-guile)
(provide 'lilypond-ts-repl)
;;; lilypond-ts-repl.el end here
