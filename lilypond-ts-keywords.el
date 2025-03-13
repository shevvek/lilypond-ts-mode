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

(defun lilypond-ts--get-and-maybe-refresh (plist &optional force-refresh)
  (when-let* (((or force-refresh
                   (plist-get plist :needs-update)))
              ((lilypond-ts--ensure-repl))
              (scm-code (plist-get plist :scm))
              (new-data (geiser-eval--send/result `(:eval ,scm-code)))
              (wrap-element (or (plist-get plist :wrap-element)
                                #'identity)))
    (plist-put plist :needs-update nil)
    (plist-put plist :value (delq nil (mapcar wrap-element new-data))))
  (plist-get plist :value))

(defvar lilypond-ts-post-eval-hook
  nil)

(defvar lilypond-ts--lists-to-refresh
  nil)

(defun lilypond-ts--require-list-refresh ()
  (dolist (plist lilypond-ts--lists-to-refresh)
    (plist-put plist :needs-update t)))

(defmacro lilypond-ts-list (suffix)
  (let ((plist-name (intern (concat "lilypond-ts--" (symbol-name suffix)))))
    `(progn
       (add-to-list 'lilypond-ts--lists-to-refresh ,plist-name)
       (lilypond-ts--get-and-maybe-refresh ,plist-name))))

(defvar lilypond-ts--lexer-keywords
  '(;; extracted from lily-lexer.cc
    ;; override is removed as it is contextually a markup command
    ;; highlight "markup" and "markuplist" as a markup
    "accepts" "addlyrics" "alias"  "alternative" "book" "bookpart" "change"
    "chordmode" "chords" "consists" "context" "default" "defaultchild" "denies"
    "description" "drummode" "drums" "etc" "figuremode" "figures" "header"
    "layout" "lyricmode" "lyrics" "lyricsto" "midi" "name" "new" "notemode"
    "paper" "remove" "repeat" "rest" "revert" "score" "sequential" "set"
    "simultaneous" "tempo" "type" "unset" "with"))

(defvar lilypond-ts--other-keywords
  nil)

(setq lilypond-ts--other-keywords
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
        "tweak" "undo" "unfoldRepeats" "unfolded" "void" "volta"))

(defvar lilypond-ts--contexts
  '( :value nil
     :needs-update t
     :scm (ly:all-context-names)
     :wrap-element symbol-name))

(defvar lilypond-ts--grobs
  '( :value nil
     :needs-update t
     :scm (map car all-grob-descriptions)
     :wrap-element symbol-name))

(defvar lilypond-ts--translators
  '( :value nil
     :needs-update t
     :scm (map ly:translator-name (ly:get-all-translators))
     :wrap-element symbol-name))

(defvar lilypond-ts--markup-functions
  '( :value nil
     :needs-update t
     :scm (keywords-of-type (lambda (v)
                              (or (markup-function? v)
                                  (markup-list-function? v))))
     :wrap-element (lambda (sym)
                     (string-trim-right (symbol-name sym)
                                        "-markup\\(-list\\)?"))))

(defvar lilypond-ts--post-events
  '( :value nil
     :needs-update t
     :scm (keywords-of-type ly:event?)
     :wrap-element symbol-name))

(defvar lilypond-ts--event-functions
  '( :value nil
     :needs-update t
     :scm (keywords-of-type ly:event-function?)
     :wrap-element symbol-name))

(defvar lilypond-ts--grob-properties
  '( :value nil
     :needs-update t
     :scm all-backend-properties
     :wrap-element symbol-name))

(defvar lilypond-ts--translation-properties
  '( :value nil
     :needs-update t
     :scm all-translation-properties
     :wrap-element symbol-name))

(defvar lilypond-ts--music-types
  '( :value nil
     :needs-update t
     :scm (map car music-descriptions)
     :wrap-element symbol-name))

(defvar lilypond-ts--music-properties
  '( :value nil
     :needs-update t
     :scm all-music-properties
     :wrap-element symbol-name))

(defvar lilypond-ts--pitch-languages
  '( :value nil
     :needs-update t
     :scm (map car language-pitch-names)
     :wrap-element symbol-name))

(defvar lilypond-ts--clefs
  '( :value nil
     :needs-update t
     :scm (map car (filter pair? supported-clefs))
     :wrap-element identity))

(defvar lilypond-ts--musics
  '( :value nil
     :needs-update t
     :scm (keywords-of-type ly:music?)
     :wrap-element symbol-name))

(defvar lilypond-ts--music-functions
  '( :value nil
     :needs-update t
     :scm (keywords-of-type ly:music-function?)
     :wrap-element symbol-name))

(defvar lilypond-ts--markups
  '( :value nil
     :needs-update t
     :scm (keywords-of-type (lambda (o)
                              (or (markup? o)
                                  (and (markup-list? o)
                                       (pair? o)))))
     :wrap-element symbol-name))

(defvar lilypond-ts--context-defs
  '( :value nil
     :needs-update t
     :scm (keywords-of-type ly:context-def?)
     :wrap-element symbol-name))

(defvar lilypond-ts--context-mods
  '( :value nil
     :needs-update t
     :scm (keywords-of-type ly:context-mod?)
     :wrap-element symbol-name))

(defvar lilypond-ts--output-defs
  '( :value nil
     :needs-update t
     :scm (keywords-of-type ly:output-def?)
     :wrap-element symbol-name))

(defvar lilypond-ts--scheme-identifiers
  '( :value nil
     :needs-update t
     :scm (:ge completions "")
     :wrap-element identity))

(defvar lilypond-ts--scheme-modules
  '( :value nil
     :needs-update t
     :scm (:ge module-completions "")
     :wrap-element identity))

(defvar lilypond-ts--repeat-types
  '(unfold tremolo volta segno percent))

;; Candidates to add to this list can be queried by running:
;; (keywords-of-type ly:accepts-maybe-property-path?) in the Geiser REPL
(defvar lilypond-ts--context-property-functions
  '("contextPropertyCheck" "popContextProperty" "propertySet" "propertyUnset"
    "pushContextProperty" "set" "unset"))

(defvar lilypond-ts--grob-property-functions
  ;; \footnote is excluded since the grob path argument is after arguments that
  ;; are likely to include \-escaped words, so \footnote won't be found by
  ;; searching backward from the property expression to the first escaped_word.
  ;; Note that some of these accept only a grob path, not a grob-property path.
  ;; Currently, grob property completions will still be offered if the user adds
  ;; a . following the grob name.
  '("alterBroken" "applyOutput" "hide" "offset" "omit" "override"
    "overrideProperty" "parenthesize" "propertyOverride" "propertyRevert"
    "propertyTweak" "revert" "shape" "styledNoteHeads" "tweak" "vshape"))

(provide 'lilypond-ts-keywords)
;;; lilypond-ts-keywords.el ends here
