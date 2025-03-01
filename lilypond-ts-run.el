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

(require 'cl-lib)
(require 'treesit)

(defvar lilypond-ts--search-path
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

(defun lilypond-ts--find-installs (&optional reset)
  (interactive "P")
  (when reset
    (setf (multisession-value lilypond-ts--lily-installs-alist) nil))
  (dolist (dir lilypond-ts--search-path)
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

(defsubst lilypond-ts--closest-compatible-lily (ver-str)
  (cl-assoc ver-str (multisession-value lilypond-ts--lily-installs-alist)
            :test #'version<=))

(defconst lilypond-ts--version-query
  (treesit-query-compile 'lilypond '((((escaped_word) @_
                                       (:match "\\\\version" @_))
                                      :anchor
                                      (string (string_fragment) @version)))))

(defun lilypond-ts--doc-lily-version ()
  (if-let* (((treesit-parser-list (current-buffer) 'lilypond))
            (caps (treesit-query-capture 'lilypond lilypond-ts--version-query))
            (version-node (cdr (assq 'version caps))))
      (treesit-node-text version-node t)
    (cdr (assq 'version file-local-variables-alist))))

(defun lilypond-ts--find-master ()
  "If the variable `master' is bound to a string or symbol that is the name of a
readable file, return that filename. Otherwise, return the buffer filename. The
unprefixed name `master' is used for compatibility with Frescobaldi document
variables."
  (if-let* (((boundp 'master))
            (master (if (symbolp master)
                        (symbol-name master)
                      master))
            ((stringp master))
            ((file-readable-p master)))
      (expand-file-name master)
    (buffer-file-name)))

(defvar lilypond-ts-include-paths nil
  "List of directories to pass with `-I' when invoking LilyPond.")

(defvar lilypond-ts-compile-args
  `(,(format "-dinclude-settings=\"%s\""
             (file-name-concat lilypond-ts-location "scm/navigation.ily")))
  "List of arguments to pass to LilyPond when compiling.")

(defun lilypond-ts-compile ()
  (interactive)
  (let ((target (lilypond-ts--find-master)))
    (with-current-buffer (find-file-noselect target)
      (let* ((ver (lilypond-ts--doc-lily-version))
             (cmd (if ver (cadr (lilypond-ts--closest-compatible-lily ver))
                    (cadar (last (multisession-value
                                  lilypond-ts--lily-installs-alist)))))
             (includes (mapcar (lambda (dir)
                                 (format "-I \"%s\"" dir))
                               lilypond-ts-include-paths))
             (compile-command (string-join `(,(format "\"%s\"" cmd)
                                             ,@includes
                                             ,@lilypond-ts-compile-args
                                             ,(format "\"%s\"" target))
                                           " ")))
        (compile compile-command)))))
