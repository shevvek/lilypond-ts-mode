# LilyPond Tree-sitter Mode
This package provides `lilypond-ts-mode`, an Emacs major mode for [GNU LilyPond](https://lilypond.org/), using [the Tree-sitter grammar created by Nate Whetsell](https://github.com/nwhetsell/tree-sitter-lilypond/) and the built-in `treesit` feature of Emacs 30+. `lilypond-ts-mode` runs LilyPond itself as an interactive Scheme environment via [Geiser](https://www.nongnu.org/geiser/), enabling live access to LilyPond's full Scheme API from Emacs.

Currently this package is in alpha.

## Currently supported features
* Live access to LilyPond's full Scheme API from Emacs, by running LilyPond itself as the Scheme REPL via Geiser. This is usable both for `lilypond-ts-mode` files and for `scheme-mode` files.
* Interactively evaluate LilyPond code within the active REPL via `lilypond-ts-eval-region`. (Note: this is currently implemented in a way that does minimal checking for valid expression boundaries or error conditions.)
* Musical time-based code navigation: cycle through the same measure/beat in all
parts with a single command.
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
| `C-c C-r` | `lilypond-ts-eval-region` |
| `C-c C-b` | `lilypond-ts-eval-buffer-and-refresh-nav` |
| `C-c M-b` | `lilypond-ts-eval-buffer` (without refreshing nav) |
| forward-sentence | `lilypond-ts-forward-moment` |
| backward-sentence | `lilypond-ts-backward-moment` |
| forward-paragraph | `lilypond-ts-forward-same-moment` |
| backward-paragraph | `lilypond-ts-backward-same-moment` |
| `C-c C-n` | `lilypond-ts-set-goal-moment` |
| `C-u C-c C-n` | unset goal moment |

## Getting Started with Musical Navigation
In order to navigate your LilyPond code using musical timing, you first need to run `lilypond-ts-eval-buffer-and-refresh-nav` (`C-c C-b`) on your file.

Once you've done that, you can use forward/backward paragraph (by default, `M-}`/`M-{`) to move to the next music expression at the same point in musical time (or the closest earlier point, if say ViolinII doesn't have a note at the same time as ViolinI). To cycle through all the parts and end up back at the same place, use `C-c C-n` to set the *goal moment* to the musical moment at *point*. Cycling through parts should work even if they are split across multiple files.

Use forward/backward sentence (`M-e`/`M-a`) to move to the next or previous rhythmic moment in the current music expression.

By default, all variables in your file containing music expressions will be included when setting up musical navigation. If you have variables that shouldn't be included, such as lyrics or custom commands defined as music expressions, or if your project has music spread across multiple files, you will need to add an entry to `lilypond-ts-moment-eval-config`. Please refer to the variable documentation for details.

Please keep in mind that this feature is very new. Bug reports are welcome.

## Planned features
* Re-load font-lock rules whenever keyword lists are refreshed.
* Auto-completion support for symbols within Scheme code (e.g. grob interfaces, event classes).
* `\paper` block highlighting and auto-completion.
* Improved granular font-lock feature coverage of LilyPond syntax elements.

## Known issues
* Implementing parser-based thing-at-point has confused autodoc within LilyPond code. An update will fix this in the near future by re-implementing autodoc so that it doesn't rely so much on Geiser outside of Scheme syntax.
* Musical navigation currently assumes that all music expressions have the same length and begin and end at the same time. If music expressions are of unequal lengths or start/end at different times (such as with temporary divisi staves or voices), navigation will still try to cycle among them, but they may be out of order and there may be bugs.
* Eval and music navigation as currently implemented lack robust constraint checking,
so running these functions in situations violating their assumptions may have unpredictable results.
* The Geiser REPL can make Emacs unresponsive in some situations, most commonly due to very long lines having been printed in the REPL buffer. If this happens, `C-g` once or twice usually will unfreeze the Emacs UI. Clearing the Geiser REPL buffer and then restarting the REPL typically fixes the issue.

## Relation to lilypond-mode
The features below are planned eventually, but overlap somewhat with existing features of `lilypond-mode`. The ideal would be to merge in the infrastructure from `lilypond-mode` that doesn't conflict with the Tree-sitter and Geiser-based features of `lilypond-ts-mode`, or to refactor `lilypond-mode` into a base mode that `lilypond-ts-mode` could derive from. Assistance on this would be much appreciated.

* Customization options
* LilyPond installation detection and version selection
* LilyPond project compilation and preview, including layout control options
* PDF point-and-click handling
* Dropdown menu

## Customization
To pass program options to the LilyPond REPL, prepend them to `ly-guile-args`. Note that spaces included in option strings will be passed literally. To add an include directory, for example, add to `ly-guile-args` either: `"-Id:/lilypond-includes/"` or `"-I" "d:/lilypond-includes/"` but not `"-I d:/lilypond-includes/"`.

To adjust which words other than lexer keywords receive *keyword* highlighting, modify `lilypond-ts--other-keywords`.

As with all Tree-sitter modes, font lock features can be toggled selectively.

The current font lock rules were developed using `gruvbox`. Feedback is welcome on how highlighting looks under other themes.

## License
`lilypond-ts-mode` is licensed under GPL-3+.

I don't believe there is any substantial code from `geiser` or `geiser-guile`, but any code from these packages retains their respective licenses; both `geiser` and `geiser-guile` are licensed under BSD 3-clause.
