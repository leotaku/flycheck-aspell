# flymake-aspell

[![GitHub License](https://img.shields.io/github/license/leotaku/flycheck-aspell?logo=none&style=flat)](https://spdx.org/licenses/GPL-3.0-or-later.html)
[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/leotaku/flycheck-aspell/check.yml?logo=none&style=flat)](https://github.com/leotaku/flycheck-aspell/actions)
[![flymake-aspell on MELPA](https://melpa.org/packages/flymake-aspell-badge.svg)](https://melpa.org/#/flymake-aspell)

## Basic usage

Simply run `flymake-aspell-setup` and then enable `flymake-mode` in any buffer you would like to spell check.
If you want to enable spell checking by default for some mode, simply add `flymake-aspell-setup` to its setup hooks.

```emacs-lisp
(require 'flycheck-aspell)
(add-hook 'text-mode-hook #'flymake-aspell-setup)
```

+ The dictionary the checkers use is determined by the values of `ispell-local-dictionary` or, if the former variable is unset, `ispell-dictionary`.
+ It might be wise to skim the [Flymake docs](https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html) to learn how to best use and configure Flymake.
+ You of course also need to install the GNU Aspell binary.

## Configuration

For seamless Emacs Ispell integration, I recommend setting the following variables.

```emacs-lisp
(setq ispell-dictionary "your_default_dictionary")
(setq ispell-program-name "aspell")
(setq ispell-silently-savep t)
```
