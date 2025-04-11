;;; lilypond-ts-keywords.el --- Dynamic keyword lists -*- lexical-binding: t -*-

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

;; Retrieve and update keyword lists via the Geiser LilyPond REPL, for font lock
;; and completion.

;;; Code:

(require 'lilypond-ts-base)
(require 'lilypond-ts-repl)

(defgroup lilypond-ts-font-lock nil
  "Font lock settings for `lilypond-ts-mode'."
  :group 'lilypond-ts)

(defcustom lilypond-ts--other-keywords
  '("absolute" "acciaccatura" "after" "afterGrace" "alterBroken"
    "appendToTag" "applyContext" "applyMusic" "applyOutput" "appoggiatura"
    "autoChange" "cadenzaOff" "cadenzaOn" "compoundMeter"
    "contextPropertyCheck" "cueDuring" "cueDuringWithClef" "fixed" "grace"
    "hide" "keepWithTag" "language" "languageRestore"
    "languageSaveAndChange" "markupMap" "omit" "once" "ottava"
    "overrideProperty" "parallelMusic" "partCombine" "partial"
    "popContextProperty" "propertyOverride" "propertyRevert" "propertySet"
    "propertyTweak" "propertyUnset" "pushContextProperty" "pushToTag"
    "quoteDuring" "relative" "removeWithTag" "scaleDurations" "settingsFrom"
    "single" "slashedGrace" "stopStaff" "tag" "tagGroup" "temporary" "time"
    "times" "transpose" "transposedCueDuring" "transposition" "tuplet"
    "tweak" "undo" "unfoldRepeats" "unfolded" "void" "volta")
  "Extra words to font lock as \\-escaped keywords in LilyPond code."
  :group 'lilypond-ts-font-lock
  :type '(repeat string))

(defun lilypond-ts--get-keywords (category)
  "Retrieve CATEGORY from `lilypond-ts--keywords' and refresh if :needs-update.

If :scm is defined and :needs-update is non-nil, run :scm via the REPL, then map
:wrap-element data and propertize all strings with :company-kind.

If CATEGORY does not define :match-function and data is non-empty, ensure that
:match-regexp is up to date for matching any word in CATEGORY."
  (if-let* ((keyword-category (assq category lilypond-ts--keywords))
            ((plist-get keyword-category :needs-update))
            ((lilypond-ts--ensure-repl))
            (scm-code (plist-get keyword-category :scm))
            (new-data (geiser-eval--send/result `(:eval ,scm-code)))
            (wrap-element (or (plist-get keyword-category :wrap-element)
                              #'identity)))
      (let* ((kind (plist-get keyword-category :company-kind))
             (wrap-with-kind (if kind
                                 (lambda (elt)
                                   (propertize (funcall wrap-element elt)
                                               :company-kind kind))
                               wrap-element)))
        (plist-put keyword-category :needs-update nil)
        (setf (cadr keyword-category)
              (delq nil (mapcar wrap-with-kind new-data)))
        (plist-put keyword-category :match-regexp nil))
    (unless (or (not (cadr keyword-category))
                (plist-get keyword-category :match-function)
                (plist-get keyword-category :match-regexp))
      (plist-put keyword-category :match-regexp
                 (format "^%s$" (regexp-opt (cadr keyword-category)))))
    keyword-category))

(defun lilypond-ts--require-keyword-updates ()
  "Lazily refresh keywords and refontify LilyPond buffers sharing the same REPL."
  (cl-loop for buffer being the buffers
           when (and (eq 'lilypond-ts-mode
                         (buffer-local-value 'major-mode buffer))
                     (eq geiser-repl--repl
                         (buffer-local-value 'geiser-repl--repl buffer)))
           do (with-current-buffer buffer
                ;; This is a bit of wasted work if project REPLS are disabled
                ;; But doesn't seem worth optimizing for the non-default setting
                (dolist (keyword-category lilypond-ts--keywords)
                  (when (plist-get keyword-category :scm)
                    (plist-put keyword-category :needs-update t)))
                (treesit-font-lock-fontify-region (point-min) (point-max)))))

(defun lilypond-ts--match-keyword (word category)
  "Is string WORD in CATEGORY, looked up via `lilypond-ts--get-keywords'?

Attempt matching using :match-function, :match-regexp, or against the raw
keyword list, in that order of preference."
  (if-let ((keyword-category (lilypond-ts--get-keywords category))
           (matcher (or (plist-get keyword-category :match-function)
                        (plist-get keyword-category :match-regexp)))
           (word (string-trim-left word "\\\\")))
      (cond
       ((functionp matcher)
        (funcall matcher word))
       ((stringp matcher)
        (let ((case-fold-search nil))
          (string-match-p matcher word))))
    (seq-contains-p (cadr keyword-category) word)))

(defun lilypond-ts--keyword-node-predicate (&rest categories)
  (lilypond-ts--intern-lambda
   (lambda (node)
     (cl-find (treesit-node-text node t) categories
              :test #'lilypond-ts--match-keyword))))

(defun lilypond-ts--grob-property-completions (&rest friends)
  "List all grob properties consistent with grob path elements FRIENDS.

If there are any path elements after the grob type, only list grob properties
that allow nesting."
  (if (lilypond-ts--ensure-repl)
      (let ((tail (seq-drop-while
                   (lambda (friend)
                     (not (lilypond-ts--match-keyword friend 'grobs)))
                   friends)))
        (geiser-eval--send/result
         `(:eval (ly:grob-property-completions ,(car tail) ,(cadr tail)))))
    (cadr (lilypond-ts--get-keywords 'grob-properties))))

(defvar lilypond-ts--keywords
  `(( clefs nil
      :needs-update t
      :scm (map car (filter pair? supported-clefs))
      :wrap-element identity
      :company-kind enum-member)
    ( repeats ("unfold" "tremolo" "volta" "segno" "percent")
      :company-kind enum-member)
    ( units ("pt" "bp" "mm" "cm" "in")
      :company-kind unit)
    ( pitch-languages nil
      :needs-update t
      :scm (map car language-pitch-names)
      :wrap-element symbol-name
      :company-kind enum)
    ( translators nil
      :needs-update t
      :scm (map ly:translator-name (ly:get-all-translators))
      :wrap-element symbol-name
      :company-kind constructor)
    ( contexts nil
      :needs-update t
      :scm (ly:all-context-names)
      :wrap-element symbol-name
      :company-kind struct)
    ( grobs nil
      :needs-update t
      :scm (map car all-grob-descriptions)
      :wrap-element symbol-name
      :company-kind class)
    ( grob-properties nil
      :needs-update t
      :scm all-backend-properties
      :wrap-element symbol-name
      :company-kind property
      :completions-function lilypond-ts--grob-property-completions)
    ( translation-properties nil
      :needs-update t
      :scm all-translation-properties
      :wrap-element symbol-name
      :company-kind type-parameter)
    ;; Candidates to add to these lists can be queried by running:
    ;; (keywords-of-type ly:accepts-maybe-property-path?) in the Geiser REPL
    ( context-commands
      ("contextPropertyCheck" "popContextProperty" "propertySet" "propertyUnset"
       "pushContextProperty" "set" "unset"))
    ( grob-commands
      ;; \footnote is excluded since the grob path argument is after arguments
      ;; that are likely to include \-escaped words, so \footnote won't be found
      ;; by searching backward from the property expression to the first
      ;; escaped_word.  Note that some of these accept only a grob path, not a
      ;; grob-property path.  Currently, grob property completions will still be
      ;; offered if the user adds a . following the grob name.
      ("alterBroken" "applyOutput" "hide" "offset" "omit" "override"
       "overrideProperty" "parenthesize" "propertyOverride" "propertyRevert"
       "propertyTweak" "revert" "shape" "styledNoteHeads" "tweak" "vshape"))
    ( lexer-keywords
      ("include" "maininput" "version"
       ;; below are extracted from lily-lexer.cc
       ;; override is removed as it is contextually a markup command highlight
       ;; "markup" and "markuplist" as a markup
       "accepts" "addlyrics" "alias" "alternative" "book" "bookpart" "change"
       "chordmode" "chords" "consists" "context" "default" "defaultchild"
       "denies" "description" "drummode" "drums" "etc" "figuremode" "figures"
       "header" "layout" "lyricmode" "lyrics" "lyricsto" "midi" "name" "new"
       "notemode" "paper" "remove" "repeat" "rest" "revert" "score" "sequential"
       "set" "simultaneous" "tempo" "type" "unset" "with")
      :company-kind keyword)
    ( other-keywords
      nil
      :match-function (lambda (word)
                        (seq-contains-p lilypond-ts--other-keywords word))
      :company-kind keyword)
    ( markup-functions nil
      :needs-update t
      :scm (keywords-of-type (lambda (v)
                               (or (markup-function? v)
                                   (markup-list-function? v))))
      :wrap-element (lambda (sym)
                      (string-trim-right (symbol-name sym)
                                         "-markup\\(-list\\)?"))
      :company-kind method)
    ( markups nil
      :needs-update t
      :scm (keywords-of-type (lambda (o)
                               (or (markup? o)
                                   (and (markup-list? o)
                                        (pair? o)))))
      :wrap-element symbol-name
      :company-kind string)
    ( music-functions nil
      :needs-update t
      :scm (keywords-of-type ly:music-function?)
      :wrap-element symbol-name
      :company-kind function)
    ( musics nil
      :needs-update t
      :scm (keywords-of-type ly:music?)
      :wrap-element symbol-name
      :company-kind variable)
    ( post-events nil
      :needs-update t
      :scm (keywords-of-type ly:event?)
      :wrap-element symbol-name
      :company-kind variable)
    ( event-functions nil
      :needs-update t
      :scm (keywords-of-type ly:event-function?)
      :wrap-element symbol-name
      :company-kind function)
    ( context-mods nil
      :needs-update t
      :scm (keywords-of-type ly:context-mod?)
      :wrap-element symbol-name
      :company-kind interface)
    ( output-defs nil
      :needs-update t
      :scm (keywords-of-type ly:output-def?)
      :wrap-element symbol-name
      :company-kind module)
    ( context-defs nil
      :needs-update t
      :scm (keywords-of-type ly:context-def?)
      :wrap-element symbol-name
      :company-kind struct)
    ( scores nil
      :company-kind text)
    ( books nil
      :company-kind reference)
    ( music-types nil
      :needs-update t
      :scm (map car music-descriptions)
      :wrap-element symbol-name
      :company-kind event)
    ( music-properties nil
      :needs-update t
      :scm all-music-properties
      :wrap-element symbol-name
      :company-kind field)
    ( paper-variables nil
      :needs-update t
      :scm (map car all-paper-variable-descriptions)
      :wrap-element symbol-name
      :company-kind variable)
    ( scheme-identifiers nil
      :needs-update t
      :scm (:ge completions "")
      :wrap-element identity
      :company-kind t
      :match-function always)
    ( grob-interfaces nil
      :needs-update t
      :scm (sort (map car (hash-table->alist (ly:all-grob-interfaces))))
      :wrap-element symbol-name
      :company-kind interface)
    ( event-classes nil
      :needs-update t
      :scm (map car (@@ (lily) all-event-classes))
      :wrap-element symbol-name
      :company-kind event)
    ( stencil-commands nil
      :needs-update t
      :scm (ly:all-stencil-expressions)
      :wrap-element symbol-name
      :company-kind command)
    ( scheme-modules nil
      :needs-update t
      :scm (:ge module-completions "")
      :wrap-element identity
      :company-kind module)
    ( geiser-scheme nil
      :company-kind macro
      :match-function always
      :completions-function
      ,(lambda (p &rest _)
         (geiser-completion--symbol-list p)))
    ( geiser-module nil
      :company-kind module
      :match-function always
      :completions-function
      ,(lambda (p &rest _)
         (geiser-completion--module-list p)))
    ( filename nil
      :company-kind file
      :match-function always
      :completions-function
      ,(lambda (p &rest _)
         (file-name-all-completions p default-directory)))))

(provide 'lilypond-ts-keywords)
;;; lilypond-ts-keywords.el ends here
