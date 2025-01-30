# Lilypond Tree-Sitter Mode
This package provides `lilypond-ts-mode`, an Emacs major mode for Lilypond, using [the Tree-Sitter grammar created by Nate Whetsell](https://github.com/nwhetsell/tree-sitter-lilypond/) and the built-in `treesit` feature of Emacs 30+. `lilypond-ts-mode` runs Lilypond itself as an interactive Scheme environment via Geiser, enabling live access to Lilypond's full Scheme API from Emacs.

Currently this package is in alpha.

## Currently supported features
* Live access to Lilypond's full Scheme API from Emacs, by running Lilypond itself as the Scheme REPL via Geiser. This is usable both for `lilypond-ts-mode` files and for `scheme-mode` files.
* Keyword lists for font lock and auto-completion populated at runtime by Lilypond itself, not hard-coded.
* Parser based font-lock for Lilypond code, with `scheme-mode` highlighting of embedded Scheme via Geiser.
* Parser based indentation for Lilypond code, with `scheme-mode` indentation of embedded Scheme. Arbitrarily nested embeddings are supported.
* Smart type-based auto-completion for property expressions (e.g. `Staff.TextScript.whiteout`).
* Auto-completion for `\`-escaped words (e.g. `\relative`).
* Auto-documentation in both Lilypond and Scheme code for music functions, markup/markuplist functions, and Scheme primitive functions exported from the Lilypond binary.
* Basic imenu support.

## Prerequisites
* Emacs 30+ with `treesit` enabled
* Git and GCC accessible to Emacs $PATH
* Geiser and Geiser-Guile (required for completion, full highlighting, and more)

  On Windows, the easiest way to provide Git and GCC is to install MSYS2 and either install Emacs using MSYS2 or run an existing Emacs install from an MSYS2 shell. Once the treesitter grammar is installed, GCC path availability is no longer needed.

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
* Load user and project Lilypond and Scheme libraries when initializing the Lilypond Scheme REPL.
* Support for custom program options for the Lilypond REPL.
* Interactive evaluation of Lilypond code.
* Parser-based structured navigation.
* Auto-completion support for symbols within Scheme code (e.g. grob interfaces, event classes).
* Specific syntax completion and highlighting (e.g. `\clef`, `\repeat`, `\consists`, chordmode).
* `\paper` block highlighting and auto-completion.
* Improved granular font-lock feature coverage of Lilypond syntax elements.

### Non-parser-related features
All of these should eventually be supported, but I am currently **not** prioritizing their development. Many are already implemented in some form by the legacy `lilypond-mode` and may eventually be merged in, adapted, or provided via derived-mode inheritance.

* Customization options
* Mode commands
* Lilypond installation detection and version selection
* Lilypond project compilation and preview, including layout control options
* PDF point-and-click handling
* Dropdown menu

## Customization
To adjust which words other than lexer keywords receive *keyword* highlighting, modify `lilypond-ts--other-keywords`.

As with all Treesitter modes, font lock features can be toggled selectively.

The current font lock rules were developed using `gruvbox`. Feedback is welcome on how highlighting looks under other themes.

## License
`lilypond-ts-mode` is licensed under GPL-3+.

I don't believe there is any substantial code from `geiser` or `geiser-guile`, but any code from these packages retains their respective licenses; both `geiser` and `geiser-guile` are licensed under BSD 3-clause.
