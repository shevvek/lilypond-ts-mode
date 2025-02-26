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

(defvar lilypond-ts--lily-install-list nil)

(defsubst lilypond-ts--version-string-to-list (vstr)
  (mapcar #'string-to-number (split-string vstr "\\." t)))

(defun lilypond-ts--find-installs ()
  (dolist (dir lilypond-ts--search-path)
    (dolist (bin (directory-files-recursively dir lilypond-ts--bin-regex nil t))
      (when-let* ((version-str (car (ignore-error file-error
                                      (process-lines bin "--version"))))
                  (lily-version (lilypond-ts--match-version "GNU LilyPond"
                                                            version-str))
                  (guile-version (lilypond-ts--match-version "Guile"
                                                             version-str)))
        (add-to-list 'lilypond-ts--lily-install-list
                     `( :lily-cmd ,bin
                        :lily-version ,lily-version
                        :guile-version ,guile-version)))))
  (sort lilypond-ts--lily-install-list
        :key (lambda (elt) (lilypond-ts--version-string-to-list
                       (plist-get elt :lily-version)))
        :lessp #'version-list-<
        :reverse t
        :in-place t))
