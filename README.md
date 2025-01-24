# Lilypond Tree-Sitter Mode
This package provides `lilypond-ts-mode`, an Emacs major mode for Lilypond, using [the Tree-Sitter grammar created by Nate Whetsell](https://github.com/nwhetsell/tree-sitter-lilypond/) and built-in `treesit` feature of Emacs 30+.

It also includes a script to automatically generate Emacs `treesit` font-lock rules using the .scm query files included in the grammar. This script has only been tested on the Lilypond grammar, but it could potentially be applicable to Treesitter grammars generally.

Currently this package is in alpha. `lilypond-ts-mode` enables parsing of Lilypond files using the grammar, and applies automatically generated font-lock rules with heuristic font-face assignments. Indentation uses `scheme-mode` rules for embedded Scheme, and should work with multiply nested embeddings of Lilypond/Scheme. `lilypond-ts-mode` doesn't yet inherit from the `lilypond-mode` provided as part of Lilypond.
## Prerequisites
* Emacs 30+ with `treesit` enabled
* Git and GCC accessible to Emacs $PATH

  On Windows, the easiest way to provide Git and GCC is to install MSYS2 and either install Emacs using MSYS2 or run an existing Emacs install from an MSYS2 shell. Once the treesitter grammar is installed, GCC path availability is no longer needed.
## Installation
1. Clone this repository.
2. Add to your `init.el`:
   ```
   (add-to-list 'load-path <local repo location>)
   (require 'lilypond-ts-mode)
   ```
3. Clone the [Lilypond Tree-Sitter grammar](https://github.com/nwhetsell/tree-sitter-lilypond/) with `--recurse-submodules`.
4. Restart Emacs.
5. When prompted, select the local directory where you cloned the grammar repo.
## Customization
Right now, the installation sets up the major mode to use automatically generated syntax highlighting rules. But you will probably want to modify at least the automatically generated font face mapping located in `auto-queries/auto-ly-font-lock-rules.el`.

Use `(setq lilypond-ts-use-auto-queries nil)` to turn off loading of the automatically generated rules by `lilypond-ts-mode`.
