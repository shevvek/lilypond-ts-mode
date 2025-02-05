# LilyPond Tree-sitter Mode
This package provides `lilypond-ts-mode`, an Emacs major mode for [GNU LilyPond](https://lilypond.org/), using [the Tree-sitter grammar created by Nate Whetsell](https://github.com/nwhetsell/tree-sitter-lilypond/) and the built-in `treesit` feature of Emacs 30+. `lilypond-ts-mode` runs LilyPond itself as an interactive Scheme environment via [Geiser](https://www.nongnu.org/geiser/), enabling live access to LilyPond's full Scheme API from Emacs.

Currently this package is in alpha.

## Currently supported features
* Live access to LilyPond's full Scheme API from Emacs, by running LilyPond itself as the Scheme REPL via Geiser. This is usable both for `lilypond-ts-mode` files and for `scheme-mode` files.
* Interactively evaluate LilyPond code within the active REPL via `lilypond-ts-eval-region`. (Note: this is currently implemented in a way that does minimal checking for valid expression boundaries or error conditions.)
* Keyword lists for font lock and auto-completion populated at runtime by LilyPond itself, not hard-coded.
* Parser based font-lock for LilyPond code, with `scheme-mode` highlighting of embedded Scheme via Geiser.
* Parser based indentation for LilyPond code, with `scheme-mode` indentation of embedded Scheme. Arbitrarily nested embeddings are supported.
* Smart type-based auto-completion for property expressions (e.g. `Staff.TextScript.whiteout`).
* Auto-completion for `\`-escaped words (e.g. `\relative`).
* Auto-documentation in both LilyPond and Scheme code for music functions, markup/markuplist functions, and Scheme primitive functions exported from the LilyPond binary.
* Parser-based structured navigation (symbol, list, sexp, defun)
* Basic imenu support.

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

## Planned features
* Load user and project LilyPond and Scheme libraries when initializing the LilyPond Scheme REPL.
* Auto-completion support for symbols within Scheme code (e.g. grob interfaces, event classes).
* Specific syntax completion and highlighting (e.g. `\clef`, `\repeat`, `\consists`, chordmode).
* `\paper` block highlighting and auto-completion.
* Improved granular font-lock feature coverage of LilyPond syntax elements.

### Non-parser-related features
All of these should eventually be supported, but I am currently **not** prioritizing their development. Many are already implemented in some form by the legacy `lilypond-mode` and may eventually be merged in, adapted, or provided via derived-mode inheritance.

* Customization options
* Mode commands
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
