;;; lilypond-ts-nav-tests.el --- Regression tests -*- lexical-binding: t -*-

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

;; lilypond-ts-navigation-mode tests.

;;; Code:

(require 'lilypond-ts-mode)
(require 'ert)
(require 'ert-x)

(defconst lilypond-ts-test--nav-overlay-ref
  '((89 108 0 1)     (108 115 4 1)    (135 140 0 2) (140 142 0.25 2)
    (142 144 0.5 2)  (144 147 0.75 2) (167 171 0 3) (171 173 0.25 3)
    (173 175 0.5 3)  (175 179 0.75 3) (179 181 1 3) (181 183 1.25 3)
    (183 185 1.5 3)  (185 204 1.75 3) (204 206 2 3) (206 208 2.25 3)
    (208 210 2.5 3)  (210 218 2.75 3) (218 221 3 3) (221 224 3.25 3)
    (224 233 3.75 3) (233 241 4 3)    (261 264 2 4) (264 266 2.25 4)
    (266 268 2.5 4)  (268 276 2.75 4) (276 278 3 4) (278 280 3.25 4)
    (280 282 3.5 4)  (282 290 3.75 4) (290 298 4 4) (306 309 4.25 4)
    (309 313 4.5 4)  (313 315 4.75 4) (315 317 5 4) (317 321 5.25 4)
    (321 323 5.5 4)  (323 325 5.75 4) (325 329 6 4) (329 331 6.25 4)
    (331 333 6.5 4)  (333 337 6.75 4) (337 342 7 4)))

(defconst lilypond-ts-test--nav-table-ref
  (let ((f1 (ert-resource-file "nav-tests-temp/moment-navigation.ly"))
        (f2 (ert-resource-file "nav-tests-temp/moment-navigation-2.ily")))
    `((((7 . 7.75)     (,f2 14 2 2)   (0 9))
       ((6.875 . 7)    (,f2 13 12 12) (0.625 8))
       ((6.75 . 6.875) (,f2 13 10 10) (0.5 8))
       ((6.625 . 6.75) (,f2 13 8 8)   (0.375 8))
       ((6.5 . 6.625)  (,f2 13 6 6)   (0.25 8))
       ((6.375 . 6.5)  (,f2 13 4 4)   (0.125 8))
       ((6.25 . 6.375) (,f2 13 2 2)   (0 8))
       ((6.125 . 6.25) (,f2 12 12 12) (0.625 7))
       ((6 . 6.125)    (,f2 12 10 10) (0.5 7))
       ((5.875 . 6)    (,f2 12 8 8)   (0.375 7))
       ((5.75 . 5.875) (,f2 12 6 6)   (0.25 7))
       ((5.625 . 5.75) (,f2 12 4 4)   (0.125 7))
       ((5.5 . 5.625)  (,f2 12 2 2)   (0 7))
       ((5.375 . 5.5)  (,f2 11 13 13) (0.625 6))
       ((5.25 . 5.375) (,f2 11 11 11) (0.5 6))
       ((5.125 . 5.25) (,f2 11 8 8)   (0.375 6))
       ((5 . 5.125)    (,f2 11 6 6)   (0.25 6))
       ((4.875 . 5)    (,f2 11 4 4)   (0.125 6))
       ((4.75 . 4.875) (,f2 11 2 2)   (0 6))
       ((4.625 . 4.75) (,f2 10 13 13) (0.625 5))
       ((4.5 . 4.625)  (,f2 10 11 11) (0.5 5))
       ((4.375 . 4.5)  (,f2 10 9 9)   (0.375 5))
       ((4.25 . 4.375) (,f2 10 7 7)   (0.25 5))
       ((4.125 . 4.25) (,f2 10 5 5)   (0.125 5))
       ((4 . 4.125)    (,f2 10 2 2)   (0 5))
       ((3.5 . 4)      (,f2 9 4 4)    (0.5 4))
       ((3 . 3.5)      (,f2 9 2 2)    (0 4))
       ((2.5 . 3)      (,f2 8 4 4)    (0.5 3))
       ((2 . 2.5)      (,f2 8 2 2)    (0 3))
       ((1.5 . 2)      (,f2 7 4 4)    (0.5 2))
       ((1 . 1.5)      (,f2 7 2 2)    (0 2))
       ((0.5 . 1)      (,f2 6 5 5)    (0.5 1))
       ((0 . 0.5)      (,f2 6 2 2)    (0 1)))
      (((4 . 7)        (,f1 8 2 2)    (0 5))
       ((0 . 4)        (,f1 6 2 2)    (0 1)))
      (((0.75 . 1)     (,f1 12 11 11) (0.75 1))
       ((0.5 . 0.75)   (,f1 12 9 9)   (0.5 1))
       ((0.25 . 0.5)   (,f1 12 7 7)   (0.25 1))
       ((0 . 0.25)     (,f1 12 2 2)   (0 1)))
      (((4 . 4.25)     (,f1 22 6 6)   (0 5))
       ((3.75 . 4)     (,f1 21 12 12) (0.75 4))
       ((3.25 . 3.75)  (,f1 21 9 9)   (0.25 4))
       ((3 . 3.25)     (,f1 21 6 6)   (0 4))
       ((2.75 . 3)     (,f1 20 12 12) (0.75 3))
       ((2.5 . 2.75)   (,f1 20 10 10) (0.5 3))
       ((2.25 . 2.5)   (,f1 20 8 8)   (0.25 3))
       ((2 . 2.25)     (,f1 20 6 6)   (0 3))
       ((1.75 . 2)     (,f1 17 8 8)   (0.75 2))
       ((1.5 . 1.75)   (,f1 17 6 6)   (0.5 2))
       ((1.25 . 1.5)   (,f1 17 4 4)   (0.25 2))
       ((1 . 1.25)     (,f1 17 2 2)   (0 2))
       ((0.75 . 1)     (,f1 16 10 10) (0.75 1))
       ((0.5 . 0.75)   (,f1 16 8 8)   (0.5 1))
       ((0.25 . 0.5)   (,f1 16 6 6)   (0.25 1))
       ((0 . 0.25)     (,f1 16 2 2)   (0 1)))
      (((7 . 7.75)     (,f1 35 2 2)   (0 9))
       ((6.75 . 7)     (,f1 34 6 6)   (0.5 8))
       ((6.5 . 6.75)   (,f1 34 4 4)   (0.25 8))
       ((6.25 . 6.5)   (,f1 34 2 2)   (0 8))
       ((6 . 6.25)     (,f1 33 6 6)   (0.5 7))
       ((5.75 . 6)     (,f1 33 4 4)   (0.25 7))
       ((5.5 . 5.75)   (,f1 33 2 2)   (0 7))
       ((5.25 . 5.5)   (,f1 32 6 6)   (0.5 6))
       ((5 . 5.25)     (,f1 32 4 4)   (0.25 6))
       ((4.75 . 5)     (,f1 32 2 2)   (0 6))
       ((4.5 . 4.75)   (,f1 31 5 5)   (0.5 5))
       ((4.25 . 4.5)   (,f1 31 2 2)   (0.25 5))
       ((4 . 4.25)     (,f1 28 6 6)   (0 5))
       ((3.75 . 4)     (,f1 27 12 12) (0.75 4))
       ((3.5 . 3.75)   (,f1 27 10 10) (0.5 4))
       ((3.25 . 3.5)   (,f1 27 8 8)   (0.25 4))
       ((3 . 3.25)     (,f1 27 6 6)   (0 4))
       ((2.75 . 3)     (,f1 26 13 13) (0.75 3))
       ((2.5 . 2.75)   (,f1 26 11 11) (0.5 3))
       ((2.25 . 2.5)   (,f1 26 9 9)   (0.25 3))
       ((2 . 2.25)     (,f1 26 6 6)   (0 3))))))

(defun lilypond-ts-test--read-nav-overlays (&optional buffer)
  "List (START END MOMENT INDEX) for BUFFER or the current buffer's overlays."
  (cl-loop for ov being the overlays of (or buffer (current-buffer))
           for m = (overlay-get ov :moment)
           collect (list (overlay-start ov)
                         (overlay-end ov)
                         m
                         (overlay-get ov :index))))

(ert-deftest lilypond-ts-test--navigation-tests ()
  (let* ((score-id nil)
         (compilation-done nil)
         (compilation-finish-functions (list (lambda (&rest _)
                                               (setq compilation-done t))))
         (ref-dir (ert-resource-file "nav-tests/"))
         (temp-dir (ert-resource-file "nav-tests-temp/"))
         (nav-dir (ert-resource-file "nav-tests-temp/.nav/"))
         (file1 (ert-resource-file "nav-tests-temp/moment-navigation.ly"))
         (file2 (ert-resource-file "nav-tests-temp/moment-navigation-2.ily")))
    (unwind-protect
        (progn
          (copy-directory ref-dir temp-dir nil nil t)
          (with-current-buffer (find-file-noselect file1)
            (lilypond-ts-mode)
            (should (file-directory-p temp-dir))
            (should (assoc temp-dir lilypond-ts--watchers #'file-equal-p))
            (lilypond-ts-compile-score)
            (while (not compilation-done)
              (sit-for 0.01))
            (should (file-exists-p
                     (ert-resource-file "nav-tests-temp/moment-navigation.pdf")))
            (should (file-exists-p nav-dir))
            (read-event nil nil 0.01)
            (should (assoc nav-dir lilypond-ts--watchers #'file-equal-p))
            (should (equal (lilypond-ts-test--read-nav-overlays)
                           lilypond-ts-test--nav-overlay-ref))
            (should (equal lilypond-ts-test--nav-table-ref
                           (cdr (assq (setq score-id
                                            (get-char-property 89 :score-id))
                                      lilypond-ts--moment-navigation-table))))
            (save-excursion
              (goto-char 233)
              (lilypond-ts-forward-same-moment 1)
              (should (eq (point) 290)))
            (save-excursion
              (goto-char 266)
              (lilypond-ts-backward-same-moment 1)
              (should (eq (point) 208)))
            (save-excursion
              (goto-char 233)
              (lilypond-ts-backward-same-moment 1)
              (should (eq (point) 108)))
            (save-excursion
              (goto-char 233)
              (lilypond-ts-forward-moment 1)
              (should (eq (point) 233)))
            (save-excursion
              (goto-char 200)
              (lilypond-ts-backward-moment 1)
              (should (eq (point) 183)))
            (unwind-protect
                (save-excursion
                  (goto-char 140)
                  (lilypond-ts-set-goal-moment)
                  (should (assq score-id lilypond-ts--goal-moments))
                  (lilypond-ts-backward-same-moment 1)
                  (should (eq (point) 89))
                  (lilypond-ts-forward-same-moment 1)
                  (should (eq (point) 140))
                  (lilypond-ts-set-goal-moment t)
                  (should-not (assq score-id lilypond-ts--goal-moments)))
              (setf (alist-get score-id lilypond-ts--goal-moments nil t) nil))
            (save-excursion
              (goto-char 295)
              (lilypond-ts-forward-same-moment 1)
              (should (eq (point) 98))
              (should (file-equal-p (buffer-file-name (current-buffer)) file2))
              (lilypond-ts-forward-same-moment 1)
              (should (file-equal-p (buffer-file-name (current-buffer)) file1))
              (should (eq (point) 108)))))
      (message "Cleaning up from navigation tests")
      (kill-buffer (find-buffer-visiting file1))
      (kill-buffer (find-buffer-visiting file2))
      (setf (alist-get score-id lilypond-ts--moment-navigation-table nil t) nil)
      (lilypond-ts--maybe-remove-nav-watcher temp-dir)
      (delete-directory temp-dir t)
      (should-not (assoc temp-dir lilypond-ts--watchers #'string-match-p)))))

(provide 'lilypond-ts-nav-tests)
;;; lilypond-ts-nav-tests.el ends here
