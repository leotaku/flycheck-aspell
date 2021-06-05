# Emacs spell check using Aspell

[![GitHub License](https://img.shields.io/github/license/leotaku/flycheck-aspell?logo=none&style=flat)](https://spdx.org/licenses/GPL-3.0-or-later.html)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/leotaku/flycheck-aspell/check?logo=none&style=flat)](https://github.com/leotaku/flycheck-aspell/actions)
[![flycheck-aspell on MELPA](https://melpa.org/packages/flycheck-aspell-badge.svg)](https://melpa.org/#/flycheck-aspell)
[![flymake-aspell on MELPA](https://melpa.org/packages/flymake-aspell-badge.svg)](https://melpa.org/#/flymake-aspell)

**NOTE:** This repository now also contains the `flymake-aspell` package, which is what I currently use for spellchecking.
Both packages should remain usable for the foreseeable future.

**NOTE:** Both `flycheck-aspell` and `flymake-aspell` now require at least [GNU Aspell 0.60.8](http://aspell.net/man-html/ChangeLog.html) for spell-checking Markdown buffers.
This is because GNU Aspell added a new filter for Markdown syntax but does not support a graceful fallback when filters are not available.

![flycheck-aspell in action](screenshot.png)

This package adds support for spell checking to Flycheck and Flymake using the [GNU Aspell](http://aspell.net) application.

## Installation

Thanks to [Sam Kleinman](https://github.com/tychoish) and [Chris Rayner](https://github.com/riscy) both `flycheck-aspell` and `flymake-aspell` can now be found on MELPA.

## Usage

+ [README-FLYMAKE.md](./README-FLYMAKE.md) :: For Flymake users
+ [README-FLYCHECK.md](./README-FLYCHECK.md) :: For Flycheck users

## Differences to `flyspell-mode`

Emacs already contains a mode for spell checking called `flyspell-mode`. It works by spell-checking words as you type, which allows for constant performance with arbitrarily large files, but also comes with a number of disadvantages. In particular, it cannot highlight misspelled words that have not been near the cursor in your current editing session. Accessing a list of all misspelled words is also impossible. Lastly, because `flyspell-mode` has to run code on every keystroke, it can often cause noticeable input delay and stuttering.

In contrast, `flycheck-aspell` and `flymake-aspell` asynchronously run Aspell in the background and extract a list of all misspelled words in the full document. This ensures a list of misspelled words is always available, which can be accessed with e.g. `flymake-show-diagnostics-buffer` or `flycheck-list-errors`. However, this also means that spell checking performance will slow down approximately linearly with the number of spelling errors. If your file is particularly large it might take a few seconds for new misspelled words to be highlighted. Note that this should never affect editing experience as no additional code is run on user input.

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
