Note: this file is auto converted from flycheck-aspell.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!

- [flycheck-aspell](#org24a0f1a)
  - [Installation](#orge30dece)
  - [Usage](#org1352d2e)
  - [Configuration](#org4a048bd)
  - [Features](#org02f84ff)


<a id="org24a0f1a"></a>

# flycheck-aspell

![flycheck-aspell in action](screenshot.png)

This package adds support for spell checking to flycheck using the [GNU aspell](http://aspell.net) application.

It is a successor (and complete rewrite) to my [flycheck-hunspell](https://github.com/leotaku/flycheck-hunspell) project, which was crippled by the bad performance of hunspell when used with larger files. (aspell performs aproximately 30x faster in the cases I tested.)

Aspell also seems to be a bit more flexible than hunspell with regard to filters, which might prove to be useful in the future.


<a id="orge30dece"></a>

## Installation

I recommend using [straight.el](https://github.com/raxod502/straight.el) for installing non-(m)elpa sources.


<a id="org1352d2e"></a>

## Usage

Simply register your preferred checkers with flycheck and then start `flycheck-mode` in the buffer you would like to spell-check. (see [1.4](#org02f84ff) for supported filetypes)

```elisp
(require 'flycheck-aspell)
(add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
```

The dictionary the checkers use is determined by the value of \`ispell-local-dictionary\` or \`ispell-dictionary\`.

It might be wise to skim the [flycheck docs](https://www.flycheck.org/en/latest/) to learn how to efficently use and configure flycheck.

You of course also need to install the `aspell` binary. All major linux distributions package it and there's probably a working macport or something.


<a id="org4a048bd"></a>

## Configuration

For seamless ispell integration, I recommend setting the following variables:

```elisp
(setq ispell-dictionary "some_dictionary"
      ispell-program-name "aspell"
      ispell-silently-savep t)
```

[This post](https://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html) might also be of interest.

You may also want to advice \`ispell-pdict-save\` for instant feedback when inserting new entries into your local dictionary:

```elisp
(advice-add 'ispell-pdict-save :after 'flycheck-maybe-recheck)
(defun flycheck-maybe-recheck (_)
  (when (bound-and-true-p flycheck-mode)
   (flycheck-buffer))
```


<a id="org02f84ff"></a>

## TODO Features

-   [X] initial featureset
-   [X] checkers for all filters (all with url support)
    -   [X] TeX
    -   [X] markdown
    -   [X] nroff
    -   [X] html
    -   [X] texinfo
    -   [X] email (message-mode)
-   [ ] tests
-   [X] honor ispell localwords (they are marked as info)
