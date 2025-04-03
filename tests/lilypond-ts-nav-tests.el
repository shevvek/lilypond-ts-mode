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
  '((89 135 nil nil 1)     (89 108 0 4 nil)        (108 115 4 7 nil)
    (135 167 nil nil 2)    (135 140 0 0.25 nil)    (140 142 0.25 0.5 nil)
    (142 144 0.5 0.75 nil) (144 147 0.75 1 nil)    (167 261 nil nil 3)
    (167 171 0 0.25 nil)   (171 173 0.25 0.5 nil)  (173 175 0.5 0.75 nil)
    (175 179 0.75 1 nil)   (179 181 1 1.25 nil)    (181 183 1.25 1.5 nil)
    (183 185 1.5 1.75 nil) (185 204 1.75 2 nil)    (204 206 2 2.25 nil)
    (206 208 2.25 2.5 nil) (208 210 2.5 2.75 nil)  (210 218 2.75 3 nil)
    (218 221 3 3.25 nil)   (221 224 3.25 3.75 nil) (224 233 3.75 4 nil)
    (233 241 4 4.25 nil)   (261 342 nil nil 4)     (261 264 2 2.25 nil)
    (264 266 2.25 2.5 nil) (266 268 2.5 2.75 nil)  (268 276 2.75 3 nil)
    (276 278 3 3.25 nil)   (278 280 3.25 3.5 nil)  (280 282 3.5 3.75 nil)
    (282 290 3.75 4 nil)   (290 298 4 4.25 nil)    (306 309 4.25 4.5 nil)
    (309 313 4.5 4.75 nil) (313 315 4.75 5 nil)    (315 317 5 5.25 nil)
    (317 321 5.25 5.5 nil) (321 323 5.5 5.75 nil)  (323 325 5.75 6 nil)
    (325 329 6 6.25 nil)   (329 331 6.25 6.5 nil)  (331 333 6.5 6.75 nil)
    (333 337 6.75 7 nil)   (337 342 7 7.75 nil)))

(defconst lilypond-ts-test--full-nav-line-ref
  (rx "  Position (1 : 0)    (-0 : -0.25) -0.25 <"
      (* "=") "|G|" (* "=")
      "> +3.75    Goal (1 : 0.25)  "))

(defun lilypond-ts-test--read-nav-overlays (&optional buffer)
  "List (START END MOMENT INDEX) for BUFFER or the current buffer's overlays."
  (cl-loop for ov being the overlays of (or buffer (current-buffer))
           collect (list (overlay-start ov)
                         (overlay-end ov)
                         (overlay-get ov :moment)
                         (overlay-get ov :end-moment)
                         (overlay-get ov :nav-index))))

(defun lilypond-ts-test--apply-char-offsets (pos offsets)
  (cl-loop for (threshold offset) on offsets by #'cddr
           when (< threshold pos) sum offset into total-offset
           finally return (+ pos total-offset)))

(defun lilypond-ts-test--modify-buffer-saving-offsets (&rest modifications)
  (cl-loop for (pos str) on modifications by #'cddr
           do (goto-char pos)
           do (insert str)
           collect pos
           collect (length str)))

(defun lilypond-ts-test--up-down-moment (&rest offsets)
  (ert-info ("up moment" :prefix "Nav test: ")
    (save-excursion
      (goto-char (lilypond-ts-test--apply-char-offsets 233 offsets))
      (lilypond-ts-up-moment 1)
      (should (eq (point) (lilypond-ts-test--apply-char-offsets 290 offsets)))))
  (ert-info ("down moment" :prefix "Nav test: ")
    (save-excursion
      (goto-char (lilypond-ts-test--apply-char-offsets 266 offsets))
      (lilypond-ts-down-moment 1)
      (should (eq (point) (lilypond-ts-test--apply-char-offsets 208 offsets)))))
  (ert-info ("down moment across voice boundary"
             :prefix "Nav test: ")
    (save-excursion
      (goto-char (lilypond-ts-test--apply-char-offsets 233 offsets))
      (lilypond-ts-down-moment 1)
      (should (eq (point) (lilypond-ts-test--apply-char-offsets 108 offsets))))))

(defun lilypond-ts-test--forward-back-moment (&rest offsets)
  (ert-info ("forward moment" :prefix "Nav test: ")
    (save-excursion
      (goto-char (lilypond-ts-test--apply-char-offsets 138 offsets))
      (lilypond-ts-forward-moment 1)
      (should (eq (point) (lilypond-ts-test--apply-char-offsets 140 offsets)))))
  (ert-info ("backward moment" :prefix "Nav test: ")
    (save-excursion
      (goto-char (lilypond-ts-test--apply-char-offsets 140 offsets))
      (lilypond-ts-backward-moment 1)
      (should (eq (point) (lilypond-ts-test--apply-char-offsets 135 offsets)))))
  (ert-info ("forward moment across voice boundary"
             :prefix "Nav test: ")
    (save-excursion
      (goto-char (lilypond-ts-test--apply-char-offsets 291 offsets))
      (lilypond-ts-forward-moment 1)
      (should (eq (point) (lilypond-ts-test--apply-char-offsets 309 offsets)))))
  (ert-info ("backward moment across voice boundary"
             :prefix "Nav test: ")
    (save-excursion
      (goto-char (lilypond-ts-test--apply-char-offsets 200 offsets))
      (lilypond-ts-backward-moment 1)
      (should (eq (point) (lilypond-ts-test--apply-char-offsets 185 offsets))))))

(defun lilypond-ts-test--forward-back-measure (&rest offsets)
  (ert-info ("forward measure" :prefix "Nav test: ")
    (save-excursion
      (goto-char (lilypond-ts-test--apply-char-offsets 208 offsets))
      (lilypond-ts-forward-measure 1)
      (should (eq (point) (lilypond-ts-test--apply-char-offsets 218 offsets)))))
  (ert-info ("forward measure across voice boundary"
             :prefix "Nav test: ")
    (save-excursion
      (goto-char (lilypond-ts-test--apply-char-offsets 179 offsets))
      (lilypond-ts-forward-measure 1)
      (should (eq (point) (lilypond-ts-test--apply-char-offsets 204 offsets)))))
  (ert-info ("backward measure multiple measures"
             :prefix "Nav test: ")
    (save-excursion
      (goto-char (lilypond-ts-test--apply-char-offsets 108 offsets))
      (lilypond-ts-backward-measure 1)
      (should (eq (point) (lilypond-ts-test--apply-char-offsets 89 offsets)))))
  (ert-info ("backward measure mid measure"
             :prefix "Nav test: ")
    (save-excursion
      (goto-char (lilypond-ts-test--apply-char-offsets 333 offsets))
      (lilypond-ts-backward-measure 1)
      (should (eq (point) (lilypond-ts-test--apply-char-offsets 329 offsets))))))

(defun lilypond-ts-test--up-moment-across-files (file1 file2 &rest offsets)
  (ert-info ("up moment across files" :prefix "Nav test: ")
    (save-excursion
      (goto-char (lilypond-ts-test--apply-char-offsets 295 offsets))
      (lilypond-ts-up-moment 1)
      (should (file-equal-p (buffer-file-name (current-buffer)) file2))
      (should (eq (point) 98))
      (lilypond-ts-up-moment 1)
      (should (file-equal-p (buffer-file-name (current-buffer)) file1))
      (should (eq (point) (lilypond-ts-test--apply-char-offsets 108 offsets))))))

(defun lilypond-ts-test--goal-moment (score-id &rest offsets)
  (unwind-protect
      (save-excursion
        (goto-char (lilypond-ts-test--apply-char-offsets 140 offsets))
        (lilypond-ts-set-goal-moment)
        (should (assq score-id lilypond-ts--goal-moment-alist))
        (lilypond-ts-down-moment 1)
        (ert-info ("down moment with goal" :prefix "Nav test: ")
          (should (eq (point) (lilypond-ts-test--apply-char-offsets 89 offsets))))
        (unless noninteractive
          (ert-info ("header line with duration bar" :prefix "Nav test: ")
            (message "%s" (format-mode-line
                           (lilypond-ts--rhythmic-position-line)))
            (should (string-match-p lilypond-ts-test--full-nav-line-ref
                                    (format-mode-line
                                     (lilypond-ts--rhythmic-position-line))))))
        (lilypond-ts-up-moment 1)
        (ert-info ("up moment with goal" :prefix "Nav test: ")
          (should (eq (point) (lilypond-ts-test--apply-char-offsets 140 offsets))))
        (lilypond-ts-set-goal-moment t)
        (should-not (assq score-id lilypond-ts--goal-moment-alist)))
    (setf (alist-get score-id lilypond-ts--goal-moment-alist nil t) nil)))

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
            (should-not (file-exists-p nav-dir))
            (should (assoc temp-dir lilypond-ts--watchers #'file-equal-p))
            (lilypond-ts-compile-score)
            (while (not compilation-done)
              (read-event nil nil 0.01))
            (should (file-exists-p
                     (ert-resource-file "nav-tests-temp/moment-navigation.pdf")))
            (should (file-exists-p nav-dir))
            (read-event nil nil 0.01)
            (should (assoc nav-dir lilypond-ts--watchers #'file-equal-p))
            (should (equal (lilypond-ts-test--read-nav-overlays)
                           lilypond-ts-test--nav-overlay-ref))
            (should-not (seq-difference
                         (cdr (assq (setq score-id
                                          (get-char-property 89 :score-id))
                                    lilypond-ts--score-id-alist))
                         (list file1 file2)
                         #'file-equal-p))
            (lilypond-ts-test--up-down-moment)
            (lilypond-ts-test--forward-back-moment)
            (lilypond-ts-test--forward-back-measure)
            (lilypond-ts-test--goal-moment score-id)
            (lilypond-ts-test--up-moment-across-files file1 file2)
            (let ((offsets (with-silent-modifications
                             (lilypond-ts-test--modify-buffer-saving-offsets
                              205 "\\accent"
                              145 "\n  e1\n  f1\n  g1"
                              75 "\n% This is a comment\n"))))
              (ert-info ("with modified buffer" :prefix "Nav test: ")
                (apply #'lilypond-ts-test--up-down-moment offsets)
                (apply #'lilypond-ts-test--forward-back-moment offsets)
                (apply #'lilypond-ts-test--forward-back-measure offsets)
                (apply #'lilypond-ts-test--goal-moment score-id offsets)
                (apply #'lilypond-ts-test--up-moment-across-files
                       file1 file2 offsets)))))
      (message "Cleaning up from navigation tests")
      (kill-buffer (find-buffer-visiting file1))
      (kill-buffer (find-buffer-visiting file2))
      (setf (alist-get score-id lilypond-ts--score-id-alist nil t) nil)
      (lilypond-ts--maybe-remove-nav-watcher temp-dir)
      (delete-directory temp-dir t)
      (should-not (cl-assoc temp-dir lilypond-ts--watchers
                            :test #'string-match-p)))))

(provide 'lilypond-ts-nav-tests)
;;; lilypond-ts-nav-tests.el ends here
