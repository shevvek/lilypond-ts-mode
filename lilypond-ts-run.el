;;; lilypond-ts-run.el --- LilyPond eval and compile  -*- lexical-binding: t -*-

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

;; Originally, I had planned for lilypond-ts-mode to either merge or inherit the
;; command running infrastructure from lilypond-mode. The decision not to do so
;; warrants explanation.

;; The compile and preview infrastructure of lilypond-mode was written over 20
;; years ago and has seen miminal evolution since. It includes long and complex
;; code for constructing shell commands, guessing output filenames, detecting
;; intermediate output dependencies, and chaining together invocations of
;; lilypond, lilypond-book, midi playback, and pdf preview.

;; Some elements of the lilypond-mode design seem to me outside its appropriate
;; scope. Detecting intermediate output dependencies is better handled via make,
;; particularly given that LilyPond now supports output file redirection that
;; cannot reasonably be predicted by an external parser. There also seems to me
;; little reason not to take advantage of Emacs's built-in compilation facility.

;; In general, the design of lilypond-mode goes to great effort to eliminate
;; even minor UI friction, using what I assume were state of the art techniques
;; circa 2005, probably based on AUCTeX. Today, virtually all novice LilyPond
;; users start out with Frescobaldi. I suspect most users of LilyPond in Emacs
;; are at minimum competent Lisp programmers and likely advanced LilyPond users.
;; For lilypond-ts-mode, therefore, my philosophy is to defer implementation of
;; UI features until they are absolutely necessary. This minimizes constraints
;; on changes to underlying features, and allows time for the right UI design to
;; crystalize.

;; Eventually, handling of automatic preview refresh, lilypond-book, and even
;; MIDI playback will be added. For now, these will require either multiple user
;; commands, or user implemented hooks. A more robust UI for selecting command
;; line arguments will also eventually be added, probably based on transients,
;; and using the REPL to retrieve type and documentation information for all
;; supported options.

;; The consideration driving this initial implementation is support for moment
;; navigation. The central design difficulty in generating navigation data is
;; knowing which music expressions go together simultaneously -- and which
;; should be ignored altogether. It became clear that generating navigation data
;; via the REPL would require user-defined configuration data tantamount to a
;; separate \score block just for navigation. Why not instead use the existing
;; \score block and piggyback on something every user is guaranteed to do
;; frequently: compiling? To support that design, lilypond-ts-mode needed to add
;; a compilation feature that would inject the navigation code by default. It
;; therefore seemed appropriate to take the opportunity to implement some now
;; expected features missing from lilypond-mode: version detection and master
;; redirection.

;;; Code:

(require 'cl-lib)
(require 'treesit)

(defvar lilypond-ts-search-path
  (cond
   ((eq system-type 'windows-nt)
    '("c:/Program Files/"
      "c:/Program Files (x86)/"))
   ((eq system-type 'darwin)
    '("/Applications/"
      "~/Applications/"))
   (t exec-path)))

(defconst lilypond-ts--bin-regex
  (if (eq system-type 'windows-nt)
      "^lilypond\\.exe$"
    "^lilypond$"))

(defconst lilypond-ts--version-rx
  (rx (+ digit) (* "." (+ digit))))

(defun lilypond-ts--match-version (name-str version-str)
  (let ((version-rx (rx (seq (literal name-str) (+ blank)
                             (group (regex lilypond-ts--version-rx))))))
    (and (string-match version-rx version-str)
         (match-string 1 version-str))))

(define-multisession-variable
  lilypond-ts--lily-installs-alist nil
  "All LilyPond installations found on the system, indexed in order by version."
  :package "lilypond-ts")

(defun lilypond-ts--index-lily-install (bin-path version-str)
  "Insert the LilyPond installation with executable at BIN-PATH and `--version'
output VERSION-STR into `lilypond-ts--lily-installs-alist', preserving version
ordering. If there are existing entries with the same LilyPond version, append
an alphabetical suffix to the version string, in order to make all installations
selectable (for example if a development build and a release version are both
present)."
  (when-let* ((lily-version (lilypond-ts--match-version "GNU LilyPond"
                                                        version-str))
              (guile-version (lilypond-ts--match-version "Guile"
                                                         version-str))
              (version-dups (cl-count lily-version
                                      (multisession-value
                                       lilypond-ts--lily-installs-alist)
                                      :key (lambda (v)
                                             (string-trim-right (car v)
                                                                "[^0-9.]*"))
                                      :test #'version=))
              (version-key (if (= 0 version-dups)
                               lily-version
                             ;; with version< 2.2-1 < 2.2 < 2.2-a
                             ;; by using letters, comparison against lily file
                             ;; versions will work correctly
                             (format "%s-%c" lily-version
                                     (+ ?a version-dups -1)))))
    (setf (multisession-value lilypond-ts--lily-installs-alist)
          (cl-merge 'list (multisession-value lilypond-ts--lily-installs-alist)
                    `((,version-key ,bin-path :guile-version ,guile-version))
                    #'version< :key #'car))))

(defun lilypond-ts-find-installs (&optional reset)
  "Search for new LilyPond installs and index them by version. With universal
prefix argument RESET, clear and refresh the list."
  (interactive "P")
  (when reset
    (setf (multisession-value lilypond-ts--lily-installs-alist) nil))
  (dolist (dir lilypond-ts-search-path)
    (dolist (bin (directory-files-recursively dir lilypond-ts--bin-regex nil t))
      (when (and (file-executable-p bin)
                 (not (cl-rassoc bin (multisession-value
                                      lilypond-ts--lily-installs-alist)
                                 :key #'car :test #'file-equal-p)))
        (make-process :name "lilypond"
                      :command (list bin "--version")
                      :filter (lambda (proc str)
                                (when (= 0 (process-exit-status proc))
                                  (lilypond-ts--index-lily-install bin str))))))))

(defun lilypond-ts--closest-compatible-lily (ver)
  "Return the entry in `lilypond-ts--lily-installs-alist' with the minimum
version >= to VER. If VER is nil, return the highest installed version."
  (if ver
      (cl-assoc ver (multisession-value lilypond-ts--lily-installs-alist)
                :test #'version<=)
    (car (last (multisession-value lilypond-ts--lily-installs-alist)))))

;; Use treesitter for this because it can easily exclude commented version lines
(defconst lilypond-ts--version-query
  (treesit-query-compile 'lilypond '((((escaped_word) @_
                                       (:match "\\\\version" @_))
                                      :anchor
                                      (string (string_fragment) @version)))))

(defun lilypond-ts--doc-lily-version ()
  "Return the current buffer's LilyPond version. If a `\\version' statement is
not found, use the value of `version' if it is locally bound."
  (if-let* (((treesit-parser-list (current-buffer) 'lilypond))
            (caps (treesit-query-capture 'lilypond lilypond-ts--version-query))
            (version-node (cdr (assq 'version caps))))
      (treesit-node-text version-node t)
    (cdr (assq 'version file-local-variables-alist))))

(defun lilypond-ts--find-master ()
  "If the variable `master' is bound to a string or symbol that is the name of a
readable file, open that file and return the buffer without selecting it.
Otherwise, return the current buffer. The unprefixed name `master' is used for
compatibility with Frescobaldi document variables."
  (if-let* (((boundp 'master))
            (master (if (symbolp master)
                        (symbol-name master)
                      master))
            ((stringp master))
            ((file-readable-p master)))
      (find-file-noselect master)
    (current-buffer)))

(defvar lilypond-ts-include-paths nil
  "List of directories to pass with `-I' when invoking LilyPond.")

(defvar lilypond-ts-compile-args nil
  "List of arguments to pass to LilyPond when compiling.")

(defun lilypond-ts-compile ()
  "Compile the current buffer using the closest compatible LilyPond version
available, with arguments constructed from `lilypond-ts-include-paths' and
`lilypond-ts-compile-args'. If `master' is defined, compile that file instead.
Compilation uses Emacs `compile' but does not save the command."
  (interactive)
  (with-current-buffer (lilypond-ts--find-master)
    (let* ((ver (lilypond-ts--doc-lily-version))
           (cmd (cadr (lilypond-ts--closest-compatible-lily ver)))
           (includes (mapcar (lambda (dir)
                               (format "-I \"%s\"" dir))
                             lilypond-ts-include-paths))
           (compile-command (string-join `(,(format "\"%s\"" cmd)
                                           ,@includes
                                           ,@lilypond-ts-compile-args
                                           ,(format "\"%s\"" (buffer-file-name)))
                                         " ")))
      ;; By locally binding compile-command, it won't clobber the saved command.
      ;; That is desirable for larger projects that may use makefile for full
      ;; builds, to facilitate compiling individual LilyPond files for preview.
      ;; The convenience of compile-command history isn't that relevant here,
      ;; because the LilyPond command is built out of configurable variables.
      ;; TODO: provide a transient interface for managing LilyPond option flags
      (compile compile-command))))

(provide 'lilypond-ts-run)
;;; lilypond-ts-run.el ends here
