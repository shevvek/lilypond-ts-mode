# LilyPond Tree-sitter Mode
This package provides `lilypond-ts-mode`, an Emacs major mode for [GNU LilyPond](https://lilypond.org/) that supports "vertical" rhythm-aware navigation through music code.

`lilypond-ts-mode` uses [the Tree-sitter grammar created by Nate Whetsell](https://github.com/nwhetsell/tree-sitter-lilypond/) and the built-in `treesit` feature of Emacs 30+. `lilypond-ts-mode` runs LilyPond itself as an interactive Scheme environment via [Geiser](https://www.nongnu.org/geiser/), enabling live access to LilyPond's full Scheme API from Emacs.

`lilypond-ts-mode` is intended as a modern replacement for `lilypond-mode`.

Currently this package is in alpha.

Contributions and bug reports are very welcome.

## Currently supported features
* Navigate "vertically" through your score, cycling through the same beat in the code for each part's music, with live UI displaying the current bar number.
* Live access to LilyPond's full Scheme API from Emacs, by running LilyPond itself as the Scheme REPL via Geiser. This is usable both for `lilypond-ts-mode` files and for `scheme-mode` files.
* Automatic detection of LilyPond installations and selection of the closest compatible version when compiling.
* Interactively evaluate LilyPond code within the active REPL via `lilypond-ts-eval-region`, making any definitions available for autocompletion, autodoc, and font lock highlighting. (Note: this is currently implemented in a way that does minimal checking for valid expression boundaries or error conditions.)
* Parser based indentation for LilyPond code, with `scheme-mode` indentation of embedded Scheme. Arbitrarily nested embeddings are supported.
* Smart type-based auto-completion for property expressions (e.g. `Staff.TextScript.whiteout`).
* Auto-completion for `\`-escaped words (e.g. `\relative`).
* Auto-documentation in both LilyPond and Scheme code for music functions, markup/markuplist functions, and Scheme primitive functions exported from the LilyPond binary.
* Parser-based structured navigation (symbol, list, sexp, defun) and imenu.
* Parser based font-lock for LilyPond code and embedded Scheme.
* Keyword lists for font lock and auto-completion populated at runtime by LilyPond itself, not hard-coded.

## Prerequisites
* Emacs 30.1+ with `treesit` enabled. (Due to continuing active development of `treesit`, it is likely that `lilypond-ts-mode` will require the latest Emacs release, at least until the next Emacs major version release.)
* Git and GCC accessible to Emacs $PATH

  On Windows, the easiest way to provide Git and GCC is to install MSYS2 and either install Emacs using MSYS2 or run an existing Emacs install from an MSYS2 shell. Once the treesitter grammar is installed, GCC path availability is no longer needed.

* [Geiser](https://gitlab.com/emacs-geiser/geiser) and [Geiser-Guile](https://gitlab.com/emacs-geiser/guile)

  Both are available via non-GNU ELPA or MELPA.

## Installation
1. Ensure prerequisites are installed.
2. Clone this repository with `--recurse-submodules`.
3. Add to your `init.el`:
   ```
   (add-to-list 'load-path <local repo location>)
   (require 'lilypond-ts-mode)
   ```

## Customization
If you have LilyPond installed in a non-standard directory, add it to `lilypond-ts-search-path`, then refresh LilyPond installs. By default, `lilypond-ts-mode` won't search for new LilyPond installations on startup unless the cached list of installs is empty.

If you customize nothing else, you will probably want to add your LilyPond include folders to `lilypond-ts-default-includes` so that they can be found when you compile. To include your custom commands and functions in autocompletion, autodoc, and font lock highlighting, also add your top-level LilyPond include files themselves to `lilypond-ts-repl-includes`.

`lilypond-ts-mode` does not currently directly support MIDI or PDF preview, but it uses Emacs compilation mode. To set up automatic preview, add a callback for your preferred previewer to `compilation-finish-functions`.

[Frescobaldi document variables](https://www.frescobaldi.org/uguide#help_document_variables) are partially compatible with Emacs and `lilypond-ts-mode`. [Emacs syntax for file-local variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html) overlaps with that used by Frescobaldi, but is stricter in some ways. To be used by Emacs, the file variables comment needs to be the first line in the file, and must include `-*-` at the beginning and end. If present, `lilypond-ts-mode` will use the `master` variable to redirect compilation, similar to Frescobaldi. The syntax Frescobaldi uses for `output` unfortunately is not valid when interpreted in Emacs Lisp, so keep this one somewhere that Emacs won't try to read as a file variable. In the future it would be nice to support `output` in order to allow for preview-on-compile and full compatibility with Frescobaldi.

As with all Tree-sitter modes, font lock features can be toggled selectively. The current font lock rules were developed using `gruvbox`. Feedback is welcome on how highlighting looks under other themes.

## Keybinds
`lilypond-ts-mode` defines the following commands:

| Binding | Command |
|:--|:--|
| `C-c C-c` | `lilypond-ts-compile-score` |
| `C-c C-S-c` | `lilypond-ts-compile-parts` |
| `C-c C-r` | `lilypond-ts-eval-region` |
| `C-c C-b` | `lilypond-ts-eval-buffer` |
| forward-word | `lilypond-ts-forward-moment` |
| backward-word | `lilypond-ts-backward-moment` |
| forward-sentence | `lilypond-ts-forward-measure` |
| backward-sentence | `lilypond-ts-backward-measure` |
| forward-paragraph | `lilypond-ts-up-moment` |
| backward-paragraph | `lilypond-ts-down-moment` |
| `C-c C-n` | `lilypond-ts-set-goal-moment` |
| `C-u C-c C-n` | unset goal moment |

## Getting Started with Musical Navigation
All that you need to do to enable musical navigation is compile by running `lilypond-ts-compile-score` (by default `C-c C-c`). By default, this injects a library that adds the necessary code to generate navigation data.

Use forward/backward paragraph (by default, `M-}`/`M-{`) to move to the next music expression at the same point in musical time (or the closest earlier point, if say ViolinII doesn't have a note at the same time as ViolinI). To cycle through all the parts and end up back at the same place, use `C-c C-n` to set the *goal moment* to the musical moment at *point*. Use `C-u C-c C-n` to unset the goal moment.

Use forward/backward word (`M-f`/`M-b`) to move to the next or previous rhythmic event, or forward/backward sentence (`M-e`/`M-a`) for the next/previous measure in the current music expression.

Musical navigation should work even for projects with complex file structures, custom contexts, or polyrhythmic scores.

If your project uses its own build process such as `make`, you can enable musical navigation by including `scm/navigation.ily` from `lilypond-ts-mode`. Some extra work may be required for use cases that define `default-toplevel-book-handler`, such as `lilypond-book`.

## License
`lilypond-ts-mode` is licensed under GPL-3+.

Some boiletplate code is derived from `geiser` or `geiser-guile`. Any code from these packages retains their respective licenses; both `geiser` and `geiser-guile` are licensed under BSD 3-clause.
