;;; lilypond-ts-tests.el --- Regression tests -*- lexical-binding: t -*-

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
(require 'cl-lib)

;;; Indent

(defun lilypond-ts-test--parser-setup ()
  (or (treesit-parser-list (current-buffer) 'lilypond)
      (treesit-parser-create 'lilypond)))

(defun lilypond-ts-test--indent-test ()
  (lilypond-ts-test--parser-setup)
  (setq-local treesit-simple-indent-rules
              (treesit--indent-rules-optimize lilypond-ts--indent-rules))
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

;;; Capf

(defun lilypond-ts-test--capf-test-setup ()
  (make-local-variable 'lilypond-ts--treesit-capf-rules)
  (make-local-variable 'lilypond-ts--keywords)
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
                  collect `(should (cl-member ,c comps :test #'string-equal)))
       ,@(cl-loop for c in (ensure-list no)
                  collect `(should-not (cl-member ,c comps
                                                  :test #'string-equal))))))

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
  :expected-result :failed
  (lilypond-ts--capf-test
   "NoteHead.s" "stencil" ("slashedGrace" "start-callback")))
(ert-deftest lilypond-ts-test--context-grob-properties-capf ()
  :expected-result (if noninteractive :failed :passed)
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

;;; Autodoc

(defmacro lilypond-ts--autodoc-test (text correct)
  "Insert TEXT in a temporary buffer, then check that `lilypond-ts-mode'
autodoc matches the literal string CORRECT."
  `(with-temp-buffer
     (lilypond-ts-test--parser-setup)
     (lilypond-ts--ensure-repl)
     (insert ,(if (stringp text) text (car text)))
     ,(when (listp text)
        `(save-excursion (insert ,(cadr text))))
     (should (string-equal ,correct (lilypond-ts--eldoc-function)))))

(defconst lilypond-ts-test--ref-music-function-autodoc-text
  #("partCombine: [number-pair? chord-range = (0 . 8)] ly:music? part1 ly:music? part2 => ly:music?"
    0 11 (face geiser-font-lock-autodoc-identifier) 14 26 (face italic) 27 38
    (face geiser-font-lock-autodoc-current-arg) 50 59 (face italic) 60 65
    (face geiser-font-lock-autodoc-current-arg) 66 75 (face italic) 76 81
    (face geiser-font-lock-autodoc-current-arg) 85 94 (face italic)))

(defconst lilypond-ts-test--ref-markup-function-autodoc-text
  #("with-string-transformer: procedure? transformer cheap-markup? arg => markup?"
    0 23 (face geiser-font-lock-autodoc-identifier) 25 35 (face italic) 36 47
    (face geiser-font-lock-autodoc-current-arg) 48 61 (face italic) 62 65
    (face geiser-font-lock-autodoc-current-arg) 69 76 (face italic)))

(ert-deftest lilypond-ts-test--music-function-autodoc ()
  (lilypond-ts--autodoc-test "\\partCombine"
                             lilypond-ts-test--ref-music-function-autodoc-text))

(ert-deftest lilypond-ts-test--markup-function-autodoc ()
  (lilypond-ts--autodoc-test "\\markup\\with-string-transformer"
                             lilypond-ts-test--ref-markup-function-autodoc-text))

;;; Font lock

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
               for comment-start = (lilypond-ts--comment-start-at-point)
               unless (string-blank-p (substring line beg end))
               do (insert (cl-case beg
                            (0 (format "\n%1$s%1$s <- %s" comment-start face))
                            (1 (format "\n %1$s%1$s <- %s" comment-start face))
                            (t (format "\n%1$s%1$s%s^ %s" comment-start
                                       (string-pad "" (- beg 2) ?\s)
                                       face)))))
      (forward-line 1))
    (save-buffer)))

(ert-font-lock-deftest-file lilypond-ts-test--font-lock
    lilypond-ts-mode "font-lock-test.ly")

(ert-font-lock-deftest-file lilypond-ts-test--scheme-font-lock
    lilypond-ts-mode "scheme-font-lock.ly")

(provide 'lilypond-ts-tests)
;;; lilypond-ts-tests.el ends here
