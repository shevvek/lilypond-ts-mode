# LilyPond Tree-sitter Mode
This package provides `lilypond-ts-mode`, an Emacs major mode for [GNU LilyPond](https://lilypond.org/) that supports rhythmic position-based code navigation.

`lilypond-ts-mode` uses [the Tree-sitter grammar created by Nate Whetsell](https://github.com/nwhetsell/tree-sitter-lilypond/) and the built-in `treesit` feature of Emacs 30+. `lilypond-ts-mode` runs LilyPond itself as an interactive Scheme environment via [Geiser](https://www.nongnu.org/geiser/), enabling live access to LilyPond's full Scheme API from Emacs.

Currently this package is in alpha.

## Currently supported features
* Navigate "vertically" through your score, cycling through the same beat in the code for each part's music.
* Live access to LilyPond's full Scheme API from Emacs, by running LilyPond itself as the Scheme REPL via Geiser. This is usable both for `lilypond-ts-mode` files and for `scheme-mode` files.
* Automatic detection of LilyPond installations and selection of the closest compatible version when compiling.
* Interactively evaluate LilyPond code within the active REPL via `lilypond-ts-eval-region`. (Note: this is currently implemented in a way that does minimal checking for valid expression boundaries or error conditions.)
* Parser based indentation for LilyPond code, with `scheme-mode` indentation of embedded Scheme. Arbitrarily nested embeddings are supported.
* Smart type-based auto-completion for property expressions (e.g. `Staff.TextScript.whiteout`).
* Auto-completion for `\`-escaped words (e.g. `\relative`).
* Auto-documentation in both LilyPond and Scheme code for music functions[^1], markup/markuplist functions, and Scheme primitive functions exported from the LilyPond binary.
* Parser-based structured navigation (symbol, list, sexp, defun) and imenu.
* Parser based font-lock for LilyPond code, with `scheme-mode` highlighting of embedded Scheme via Geiser.
* Keyword lists for font lock and auto-completion populated at runtime by LilyPond itself, not hard-coded.

[^1]: Autodoc for `\`-escaped words is a bit unreliable currently. See [known issues](#known-issues).

## Prerequisites
* Emacs 30+ with `treesit` enabled
* Git and GCC accessible to Emacs $PATH

  On Windows, the easiest way to provide Git and GCC is to install MSYS2 and either install Emacs using MSYS2 or run an existing Emacs install from an MSYS2 shell. Once the treesitter grammar is installed, GCC path availability is no longer needed.

* [Geiser](https://gitlab.com/emacs-geiser/geiser) and [Geiser-Guile](https://gitlab.com/emacs-geiser/guile)

  Both are available via non-GNU ELPA or MELPA.

## Installation
1. Ensure prerequisites are installed.
2. Clone this repository.
3. Add to your `init.el`:
   ```
   (add-to-list 'load-path <local repo location>)
   (require 'lilypond-ts-mode)
   (setq ly-guile-bin <path to lilypond executable>)
   ```
## Keybinds
`lilypond-ts-mode` defines the following commands:

| Binding | Command |
|:--|:--|
| `C-c C-c` | `lilypond-ts-compile` |
| `C-c C-r` | `lilypond-ts-eval-region` |
| `C-c M-b` | `lilypond-ts-eval-buffer` |
| forward-sentence | `lilypond-ts-forward-moment` |
| backward-sentence | `lilypond-ts-backward-moment` |
| forward-paragraph | `lilypond-ts-forward-same-moment` |
| backward-paragraph | `lilypond-ts-backward-same-moment` |
| `C-c C-n` | `lilypond-ts-set-goal-moment` |
| `C-u C-c C-n` | unset goal moment |

## Getting Started with Musical Navigation
All that you need to do to enable musical navigation is compile by running `lilypond-ts-compile` (by default `C-c C-c`). By default, this injects a library that adds the necessary code to generate navigation data.

Use forward/backward paragraph (by default, `M-}`/`M-{`) to move to the next music expression at the same point in musical time (or the closest earlier point, if say ViolinII doesn't have a note at the same time as ViolinI). To cycle through all the parts and end up back at the same place, use `C-c C-n` to set the *goal moment* to the musical moment at *point*. Use `C-u C-c C-n` to unset the goal moment.

Use forward/backward sentence (`M-e`/`M-a`) to move to the next or previous rhythmic moment in the current music expression.

Musical navigation should work even for projects with complex file structures, custom contexts, or polyrhythmic scores.

If your project uses its own build process such as `make`, you can enable musical navigation by including `scm/navigation.ily` from `lilypond-ts-mode`. Some extra work may be required for use cases that define `default-toplevel-book-handler`, such as `lilypond-book`.

## Planned features
* Re-load font-lock rules whenever keyword lists are refreshed.
* Auto-completion support for symbols within Scheme code (e.g. grob interfaces, event classes).
* `\paper` block highlighting and auto-completion.
* Improved granular font-lock feature coverage of LilyPond syntax elements.

## Known issues
* Implementing parser-based thing-at-point has confused autodoc within LilyPond code. An update will fix this in the near future by re-implementing autodoc so that it doesn't rely so much on Geiser outside of Scheme syntax.
* `lilypond-ts-mode` can become very laggy at times, particularly when editing files containing embedded Scheme. There are multiple possible contributing causes: repeated calculations related to nested embeddings; repeated REPL calls during auto-completion; very long lines having been printed in the REPL buffer; custom Scheme code loaded into the REPL session. If this happens after you have been using the same REPL for a while, it may help to kill the current LilyPond Geiser REPL, clear the REPL buffer, and reload the REPL. You may also find it helpful to remove one or more of the completion-at-point functions provided by `lilypond-ts-mode`. If the Emacs UI appears frozen, `C-g` once or twice usually unfreezes it by cancelling whatever process was blocking it (frequently autocompletion). Note that if you only encounter unresponsive UI on first loading a file with `lilypond-ts-mode`, it's best to wait, as the lag is likely due to important setup processes, including search for LilyPond installations, REPL startup, population of keyword lists, and loading of navigation data. For large projects (thousands of lines across multiple files), loading musical navigation data can take a minute or more. Improving UI responsiveness is a high priority but will take a significant amount of work across multiple systems.

## Relation to lilypond-mode
Originally, the plan was to eventually merge relevant aspects of `lilypond-mode`, particularly UI features such as command handling, menus, and customization groups. See comments at the top of `lilypond-ts-run.el` for why this is no longer planned.

## Customization
To customize the options passed when compiling, modify `lilypond-ts-compile-args`. Add include directories to `lilypond-ts-include-paths`.

If you have LilyPond installed in a non-standard directory, add it to `lilypond-ts-search-path`. To refresh the list of LilyPond installations, run `M-x lilypond-ts-find-installs`. Run it with `C-u` prefix to clear the old list of installs. By default, `lilypond-ts-mode` won't search for new LilyPond installations on startup unless the cached list of installs is empty.

[Frescobaldi document variables](https://www.frescobaldi.org/uguide#help_document_variables) are partially compatible with Emacs and `lilypond-ts-mode`. [Emacs syntax for file-local variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html) overlaps with that used by Frescobaldi, but is stricter in some ways. To be used by Emacs, the file variables comment needs to be the first line in the file, and must include `-*-` at the beginning and end. If present, `lilypond-ts-mode` will use the `master` variable to redirect compilation, similar to Frescobaldi. The syntax Frescobaldi uses for `output` unfortunately is not valid when interpreted in Emacs Lisp, so keep this one somewhere that Emacs won't try to read as a file variable. In the future it would be nice to support `output` in order to allow for preview-on-compile and full compatibility with Frescobaldi.

To pass program options to the LilyPond REPL, prepend them to `ly-guile-args`. Note that spaces included in option strings will be passed literally. To add an include directory, for example, add to `ly-guile-args` either: `"-Id:/lilypond-includes/"` or `"-I" "d:/lilypond-includes/"` but not `"-I d:/lilypond-includes/"`. In the near future, this will be integrated with `lilypond-ts-run.el`.

To adjust which words other than lexer keywords receive *keyword* highlighting, modify `lilypond-ts--other-keywords`.

As with all Tree-sitter modes, font lock features can be toggled selectively.

The current font lock rules were developed using `gruvbox`. Feedback is welcome on how highlighting looks under other themes.

## License
`lilypond-ts-mode` is licensed under GPL-3+.

I don't believe there is any substantial code from `geiser` or `geiser-guile`, but any code from these packages retains their respective licenses; both `geiser` and `geiser-guile` are licensed under BSD 3-clause.
