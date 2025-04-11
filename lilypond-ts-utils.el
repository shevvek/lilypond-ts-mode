;;; lilypond-ts-utils.el --- Utility functions -*- lexical-binding: t -*-

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

;; The functions in this package are designed to be as generic as possible, and
;; could potentially be upstreamed into Emacs.

;;; Code:

(require 'treesit)

(defsubst lilypond-ts--overlay-match-p (ov plist)
  "Return non-nil if OV has every property in PLIST with a matching value."
  (cl-loop for (key value) on plist by #'cddr
           always (equal (overlay-get ov key) value)))

(defun lilypond-ts--update-overlay (tick beg end &rest props)
  "Ensure there is an overlay bounded by BEG and END with its :update-tick set
to TICK and properties PROPS. If there is an existing overlay in the interval
BEG to END with all properties and values in PROPS, move it and update its
:update-tick. Otherwise, create a new overlay."
  (let ((overlay (seq-find (lambda (ov)
                             (lilypond-ts--overlay-match-p ov props))
                           (overlays-in beg end))))
    (if overlay
        (move-overlay overlay beg end)
      (setq overlay (make-overlay beg end))
      (cl-loop for (key value) on props by #'cddr
               do (overlay-put overlay key value)
               collect key into keys
               finally (apply #'lilypond-ts--cleanup-overlays
                              tick beg end keys)))
    (overlay-put overlay :update-tick tick)))

(defun lilypond-ts--cleanup-overlays (tick &optional beg end &rest keys)
  "Remove overlays on the current buffer with :update-tick < TICK. Optionally,
only look at overlays between BEG and END. Only look at overlays that include
all properties in KEYS."
  (cl-loop for ov being the overlays
           from (or beg (point-min)) to (or end (point-max))
           for props = (overlay-properties ov)
           for old-tick = (plist-get props :update-tick)
           when (and (not (seq-difference keys props))
                     (number-or-marker-p old-tick)
                     (< old-tick tick))
           do (delete-overlay ov)))

(defun lilypond-ts--go-to-loc (file ln ch &rest col)
  "Open FILE and move point to line LN character CH."
  (when file (find-file file))
  (forward-line (- ln (line-number-at-pos (point))))
  (forward-char ch)
  (point))

(defsubst lilypond-ts--node-preceded-by-whitespace (node)
  (string-match-p "\\s-" (string (char-before (treesit-node-start node)))))

;; This is stupid and is only needed because Treesit query :pred clauses accept
;; only the interned name of a function, not the function itself.
(defun lilypond-ts--intern-lambda (l)
  "Turn lambda L into a named function using `cl-gentemp' and return the name."
  (let ((name (cl-gentemp)))
    (fset name l)
    name))

;; Probably this exists in dash.el or something...
(defsubst lilypond-ts--map-squared (fun seq)
  "Map FUN over the elements of the elements of SEQ."
  (mapcar (lambda (l)
            (mapcar fun l))
          seq))

(defun lilypond-ts--treesit-query-parents (node query &optional
                                                start-depth end-depth)
  "Try QUERY on NODE's START-DEPTH to END-DEPTH parents until the first success."
  (cl-loop repeat (if end-depth
                      (max 1 (- end-depth (or start-depth 0)))
                    max-lisp-eval-depth)
           for n = (if start-depth
                       (treesit-node-get node `((parent ,start-depth)))
                     node)
           then (treesit-node-parent n)
           always n
           thereis (treesit-query-capture n query)))

(defun lilypond-ts--treesit-isolate-capture-group (node captures)
  "Return the nodes of the first capture group containing NODE in CAPTURES.

Capture groups are distinguished by repetition of capture names."
  (cl-loop with found-node = nil
           for (capture-name . capture-node) in captures
           when (treesit-node-eq node capture-node) do (setf found-node t)
           when (memq capture-name names) if found-node return group
           else do (setf group nil) and do (setf names nil)
           collect capture-name into names
           collect capture-node into group
           finally return (and found-node group)))

(defun lilypond-ts--treesit-query-depths (query-sexp)
  "Return as a pair the min and max depth of captures in `treesit' QUERY-SEXP."
  (named-let parse-query ((query query-sexp)
                          (depth -1))
    ;; Start by iterating, since all queries are lists of patterns.
    (cl-loop with implicit-parent = 0
             for child being the elements of query
             for depths = (pcase child
                            ((and (cl-type symbol)
                                  (app symbol-name (rx "@" (+ anything))))
                             (cons depth depth))
                            ;; No need to recurse into atomic nodes.
                            (`(,(cl-type symbol))
                             nil)
                            ;; Ignore captures inside predicate clauses.
                            (`(,(or :match :pred :equal) . ,_)
                             nil)
                            ((or (cl-type vector)
                                 `(,head . ,_))
                             ;; Only increment depth if this nesting is a node.
                             ;; Initial depth = -1 is only so we don't add an
                             ;; implicit parent shared by top level patterns.
                             (parse-query child (if (and head
                                                         (symbolp head)
                                                         (not (keywordp head)))
                                                    (1+ (max 0 depth))
                                                  (max 0 depth)))))
             ;; If depth = 0 and there are multiple branches with captures, then
             ;; the branches implicitly share (_) as a parent.
             when (and depths min-depth (= 0 depth) (= 0 implicit-parent))
             do (setq implicit-parent 1)
             when depths
             minimize (car depths) into min-depth
             and maximize (cdr depths) into max-depth
             finally return (and min-depth
                                 ;; This also takes care of the shorthand case
                                 ;; of captures as top-level query elements
                                 (cons (max implicit-parent min-depth)
                                       (max implicit-parent max-depth))))))

(provide 'lilypond-ts-utils)
;;; lilypond-ts-utils.el ends here
