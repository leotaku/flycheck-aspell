# flycheck-aspell

[![GitHub License](https://img.shields.io/github/license/leotaku/flycheck-aspell?logo=none&style=flat)](https://spdx.org/licenses/GPL-3.0-or-later.html)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/leotaku/flycheck-aspell/check?logo=none&style=flat)](https://github.com/leotaku/flycheck-aspell/actions)
[![flycheck-aspell on MELPA](https://melpa.org/packages/flycheck-aspell-badge.svg)](https://melpa.org/#/flycheck-aspell)
[![flymake-aspell on MELPA](https://melpa.org/packages/flymake-aspell-badge.svg)](https://melpa.org/#/flymake-aspell)

**NOTE:** This repository now also contains the `flymake-aspell` package, which is what I currently use for spellchecking.
Documentation regarding said package can be found inside the `flymake-aspell.el` file.
Both packages should remain usable for the foreseeable future.

**NOTE:** Both `flycheck-aspell` and `flymake-aspell` now require at least [GNU Aspell 0.60.8](http://aspell.net/man-html/ChangeLog.html) for spell-checking Markdown buffers.
This is because GNU Aspell added a new filter for Markdown syntax but does not support a graceful fallback when filters are not available.

![flycheck-aspell in action](screenshot.png)

This package adds support for spell checking to Flycheck and Flymake using the [GNU Aspell](http://aspell.net) application.

It is a successor to (and complete rewrite of) my [flycheck-hunspell](https://github.com/leotaku/flycheck-hunspell) project, which was crippled by the bad performance of Hunspell when used with larger files. Aspell performs aproximately 30x faster in the cases I tested.

Aspell also seems to be a bit more flexible than Hunspell with regard to syntax filters, which might prove to be useful in the future.

## Installation

Thanks to [Sam Kleinman](https://github.com/tychoish) and [Chris Rayner](https://github.com/riscy) both `flycheck-aspell` and `flymake-aspell` can now be found on MELPA.
This should be seen as the preferred distribution method for new users, however manual installation is of course still possible and you will not have to change your existing configuration to accommodate this change.

## Usage

Simply register your preferred checkers with Flycheck and then start `flycheck-mode` in the buffer you would like to spell-check.

```emacs-lisp
(require 'flycheck-aspell)
(add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
```

The dictionary the checkers use is determined by the values of `ispell-local-dictionary` or `ispell-dictionary`.

It might be wise to skim the [Flycheck docs](https://www.flycheck.org/en/latest/) to learn how to best use and configure Flycheck.

You of course also need to install the GNU Aspell binary.

## Configuration

For seamless Ispell integration, I recommend setting the following variables:

```emacs-lisp
(setq ispell-dictionary "some_dictionary")
(setq ispell-program-name "aspell")
(setq ispell-silently-savep t)
```

[This post](https://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html) might also be of interest.

You may also want to advice `ispell-pdict-save` for instant feedback when inserting new entries into your local dictionary:

```emacs-lisp
(advice-add #'ispell-pdict-save :after #'flycheck-maybe-recheck)
(defun flycheck-maybe-recheck (_)
  (when (bound-and-true-p flycheck-mode)
   (flycheck-buffer))
```

## Features

- [X] Initial featureset
- [X] Checkers for all filters (all with url support)
  - [X] TeX
  - [X] Markdown
  - [X] Nroff
  - [X] Html
  - [X] Texinfo
  - [X] Email (message-mode)
- [ ] Tests
- [X] Honor Ispell localwords (TODO: Flycheck only)
