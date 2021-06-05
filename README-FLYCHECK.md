# flycheck-aspell

[![GitHub License](https://img.shields.io/github/license/leotaku/flycheck-aspell?logo=none&style=flat)](https://spdx.org/licenses/GPL-3.0-or-later.html)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/leotaku/flycheck-aspell/check?logo=none&style=flat)](https://github.com/leotaku/flycheck-aspell/actions)
[![flycheck-aspell on MELPA](https://melpa.org/packages/flycheck-aspell-badge.svg)](https://melpa.org/#/flycheck-aspell)

## Basic usage

Simply register your preferred checkers with Flycheck and then start `flycheck-mode` in the buffer you would like to spell-check.

```emacs-lisp
(require 'flycheck-aspell)
(add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
```

+ The dictionary the checkers use is determined by the values of `ispell-local-dictionary` or, if the former variable is unset, `ispell-dictionary`.
+ It might be wise to skim the [Flycheck docs](https://www.flycheck.org/en/latest/) to learn how to best use and configure Flycheck.
+ You of course also need to install the GNU Aspell binary.

## Configuration

For seamless Emacs Ispell integration, I recommend setting the following variables.

```emacs-lisp
(setq ispell-dictionary "your_default_dictionary")
(setq ispell-program-name "aspell")
(setq ispell-silently-savep t)
```

You may also want to advice `ispell-pdict-save` to refresh flycheck when inserting new entries into your local dictionary.
This way highlighting instantly updates when you add a previously unknown word.

```emacs-lisp
(advice-add #'ispell-pdict-save :after #'flycheck-maybe-recheck)
(defun flycheck-maybe-recheck (_)
  (when (bound-and-true-p flycheck-mode)
   (flycheck-buffer))
```
