Note: this file is auto converted from flycheck-aspell.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# Table of Contents

1.  [flycheck-aspell](#orgd1ccf98)
    1.  [IMPORTANT FOR USERS](#org39c44f0)
    2.  [Installation](#org7457e02)
    3.  [Usage](#org7c8caf2)
    4.  [Configuration](#org3026cac)
    5.  [Features](#orgafdfe3f)


<a id="orgd1ccf98"></a>

# flycheck-aspell


<a id="org39c44f0"></a>

## IMPORTANT FOR USERS

This repository now also contains the `flymake-aspell` package,
which is what I currently use for spellchecking.
Documentation regarding said package can be found in its `.el`
file.

The `flymake-aspell` package is more recent and written in a more
consistent style, which might make it faster and more featureful
than `flycheck-aspell`.

However `flycheck-aspell` should still remain usable for the
forseeable future.

![flycheck-aspell in action](screenshot.png)

This package adds support for spell checking to flycheck using
the [GNU aspell](http://aspell.net) application.

It is a successor (and complete rewrite) to my
[flycheck-hunspell](https://github.com/leotaku/flycheck-hunspell)
project, which was crippled by the bad performance of hunspell when
used with larger files.
(aspell performs aproximately 30x faster in the cases I tested.)

Aspell also seems to be a bit more flexible than hunspell with regard
to filters, which might prove to be useful in the future.


<a id="org7457e02"></a>

## Installation

I recommend using [straight.el](https://github.com/raxod502/straight.el) for
installing non-(m)elpa sources.


<a id="org7c8caf2"></a>

## Usage

Simply register your preferred checkers with flycheck and then start `flycheck-mode`
in the buffer you would like to spell-check.
(see [1.5](#orgafdfe3f) for supported filetypes)

    (require 'flycheck-aspell)
    (add-to-list 'flycheck-checkers 'tex-aspell-dynamic)

The dictionary the checkers use is determined by the value of
\`ispell-local-dictionary\` or \`ispell-dictionary\`.

It might be wise to skim the [flycheck docs](https://www.flycheck.org/en/latest/)
to learn how to efficently use and configure flycheck.

You also need to install the GNU `aspell` and `sed` binaries.
`sed` is needed for preprocessing the file that is sent to aspell
with high performance.

All major linux distributions should package these and there are
probably working macports or something.


<a id="org3026cac"></a>

## Configuration

For seamless ispell integration, I recommend setting the following variables:

    (setq ispell-dictionary "some_dictionary"
          ispell-program-name "aspell"
          ispell-silently-savep t)

[This post](https://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html)
might also be of interest.

You may also want to advice \`ispell-pdict-save\` for instant feedback when inserting
new entries into your local dictionary:

    (advice-add 'ispell-pdict-save :after 'flycheck-maybe-recheck)
    (defun flycheck-maybe-recheck (_)
      (when (bound-and-true-p flycheck-mode)
       (flycheck-buffer))


<a id="orgafdfe3f"></a>

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

