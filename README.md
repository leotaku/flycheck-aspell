Note: this file is auto converted from flycheck-aspell.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!

- [flycheck-aspell](#org25e8d31)
  - [Installation](#org654ffec)
  - [Usage](#org55973de)
  - [Configuration](#org84c4b68)
  - [Features](#orgab2426e)


<a id="org25e8d31"></a>

# flycheck-aspell

![flycheck-aspell in action](screenshot.png)

This package adds support for spell checking to flycheck using the [GNU aspell](http://aspell.net) application.

It is a successor (and complete rewrite) to my [flycheck-hunspell](https://github.com/leotaku/flycheck-hunspell) project, which was crippled by the bad performance of hunspell when used with larger files. (aspell performs aproximately 30x faster in the cases I tested.)

Aspell also seems to be a bit more flexible than hunspell with regard to filters, which might prove to be useful in the future.


<a id="org654ffec"></a>

## Installation

I recommend using [straight.el](https://github.com/raxod502/straight.el) for installing non-(m)elpa sources.


<a id="org55973de"></a>

## Usage

Simply register your preferred checkers with flycheck.

```elisp
(add-to-list 'flycheck-checkers 'tex-aspell-generic)
```

You of course also need to install the `hunspell` binary. All major linux distributions package it and there's probably a working macport or something.


<a id="org84c4b68"></a>

## Configuration

For steamless ispell integration, I recommend setting the following variables:

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


<a id="orgab2426e"></a>

## TODO Features

-   [X] initial featureset
-   [ ] checkers for all filters
    -   [X] TeX
    -   [ ] nroff
    -   [ ] html
    -   all others
-   [ ] tests
-   [ ] honor ispell localwords
