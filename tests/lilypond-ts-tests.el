;;; lilypond-ts-tsts.el --- Regression tests -*- lexical-binding: t -*-

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

;;; Code:

(require 'lilypond-ts-mode)
(require 'ert)
(require 'ert-x)

(defun lilypond-ts-test--parser-setup ()
  (or (treesit-parser-list (current-buffer) 'lilypond)
      (treesit-parser-create 'lilypond)))

(defun lilypond-ts-test--indent-test ()
  (lilypond-ts-test--parser-setup)
  (setq-local treesit-simple-indent-rules
              (treesit--indent-rules-optimize lilypond-ts-indent-rules))
  (setq-local indent-line-function #'treesit-indent)
  (setq-local indent-region-function #'treesit-indent-region)
  (setq-local lisp-indent-function #'scheme-indent-function)
  (setq-local syntax-propertize-function #'lilypond-ts--propertize-syntax)
  (indent-region-line-by-line (point-min) (point-max))
  (remove-list-of-text-properties (point-min) (point-max) '(syntax-table)))

(ert-deftest lilypond-ts--indent-tests ()
  (skip-unless (treesit-ready-p 'lilypond))
  (ert-test-erts-file (ert-resource-file "indent.erts")
                      #'lilypond-ts-test--indent-test))

(defun lilypond-ts-test--keyword-setup ()
  (dolist (key lilypond-ts--completion-categories)
    (when-let ((kw-list (intern-soft (concat "lilypond-ts--"
                                             (symbol-name (car key))))))
      (make-local-variable kw-list))))

(defun lilypond-ts-test--capf-test-setup ()
  (lilypond-ts-test--parser-setup)
  (lilypond-ts-test--keyword-setup)
  ;; need to deal with REPL
  (make-local-variable 'lilypond-ts--treesit-capf-rules)
  (make-local-variable 'lilypond-ts--treesit-completion-rules)
  (lilypond-ts-mode))

;; based on elisp--test-completions
(defun lilypond-ts-test--list-completions ()
  (let ((data (lilypond-ts--treesit-capf)))
    (all-completions (buffer-substring (nth 0 data) (nth 1 data))
                     (nth 2 data)
                     (plist-get (nthcdr 3 data) :predicate))))

(defmacro lilypond-ts--capf-test (text yes &optional no)
  "Insert TEXT in a temporary buffer, then check that `lilypond-ts-mode'
completions include YES and do not include NO. TEXT may be either a string or a
list (BEFORE-POINT AFTER-POINT). YES and NO may be either strings or lists of
strings."
  `(with-temp-buffer
     (lilypond-ts-test--capf-test-setup)
     (insert ,(if (stringp text) text (car text)))
     ,(when (listp text)
        `(save-excursion (insert ,(cadr text))))
     (let ((comps (lilypond-ts-test--list-completions)))
       ,@(cl-loop for c in (ensure-list yes)
                  collect `(should (member ,c comps)))
       ,@(cl-loop for c in (ensure-list no)
                  collect `(should-not (member ,c comps))))))

(ert-deftest lilypond-ts-test--escaped-word-capf ()
  (lilypond-ts--capf-test
   "\\tra" "transpose" "transpose-array"))
(ert-deftest lilypond-ts-test--clef-capf ()
  (lilypond-ts--capf-test
   "\\clef t" "treble" "transpose"))
(ert-deftest lilypond-ts-test--language-capf ()
  (lilypond-ts--capf-test
   "\\language e" "english" "endSpanners"))
(ert-deftest lilypond-ts-test--translator-capf ()
  (lilypond-ts--capf-test
   "\\consists V" "Volta_engraver" ("Voice" "VoiceFollower")))
(ert-deftest lilypond-ts-test--context-spec-capf ()
  (lilypond-ts--capf-test
   "\\context S" "Staff" ("StaffGrouper" "Slur_engraver")))
(ert-deftest lilypond-ts-test--repeat-capf ()
  (lilypond-ts--capf-test
   "\\repeat u" "unfold" "undo"))
(ert-deftest lilypond-ts-test--music-type-cmd-capf ()
  (lilypond-ts--capf-test
   "\\markupMap S" "SequentialMusic" ("Staff" "Slur" "Staff_performer")))
(ert-deftest lilypond-ts-test--music-properties-capf ()
  (lilypond-ts--capf-test
   "MultiMeasureRestMusic.i" "iterator-ctor" "ignore-collision"))
(ert-deftest lilypond-ts-test--set-context-capf ()
  (lilypond-ts--capf-test
   "\\set D" "Dynamics" "DynamicText"))
(ert-deftest lilypond-ts-test--set-property-capf ()
  (lilypond-ts--capf-test
   "\\set i" "instrumentName" "ignore-collision"))
(ert-deftest lilypond-ts-test--context-property-capf ()
  (lilypond-ts--capf-test
   "Voice.k" "keepAliveInterfaces" "knee"))
(ert-deftest lilypond-ts-test--override-cmd-capf ()
  (lilypond-ts--capf-test
   "\\override D" ("DynamicText" "Dynamics")))
(ert-deftest lilypond-ts-test--grob-properties-capf ()
  (lilypond-ts--capf-test
   "NoteHead.s" "stencil" ("slashedGrace" "start-callback")))
(ert-deftest lilypond-ts-test--grob-properties-capf ()
  (lilypond-ts--capf-test
   "Voice.NoteHead.s" "stencil" ("slashedGrace" "start-callback")))
(ert-deftest lilypond-ts-test--nested-grob-properties-capf ()
  (lilypond-ts--capf-test
   ("Staff.TextSpanner.b" ".left") "bound-details" "bend-me"))
(ert-deftest lilypond-ts-test--grob-capf ()
  (lilypond-ts--capf-test
   "Voice.N" "NoteHead" "Note_heads_engraver"))
(ert-deftest lilypond-ts-test--scheme-capf ()
  (lilypond-ts--capf-test
   ("#(tra" ")") ("transpose-array" "transpose")))

(defun lilypond-ts-test--generate-font-lock-assertions (file)
  "Annotate FILE with ERT font lock assertions using FILE's default mode."
  (interactive "f")
  (with-current-buffer (find-file-noselect file)
    (when (featurep 'aggressive-indent)
      (aggressive-indent-mode -1))
    (electric-indent-local-mode -1)
    (goto-char (point-min))
    (while (not (eobp))
      (goto-char (line-end-position))
      (cl-loop with line = (buffer-substring (line-beginning-position)
                                             (line-end-position))
               for (beg . end) being the intervals of line property 'face
               for face = (get-char-property beg 'face line)
               for comment-start = (if (lilypond-ts--scheme-at-p (min (1+ (point))
                                                                      (point-max)))
                                       ";;"
                                     "%%")
               unless (string-blank-p (substring line beg end))
               do (insert (cl-case beg
                            (0 (format "\n%s <- %s" comment-start face))
                            (1 (format "\n %s <- %s" comment-start face))
                            (t (format "\n%s%s^ %s" comment-start
                                       (string-pad "" (- beg 2) ?\s)
                                       face)))))
      (forward-line 1))
    (save-buffer)))

(ert-font-lock-deftest-file lilypond-ts-test--font-lock
    lilypond-ts-mode "font-lock-test.ly")

(provide 'lilypond-ts-tests)
;;; lilypond-ts-tests.el ends here
