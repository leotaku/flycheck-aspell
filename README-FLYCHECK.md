# flycheck-aspell

[![GitHub License](https://img.shields.io/github/license/leotaku/flycheck-aspell?logo=none&style=flat)](https://spdx.org/licenses/GPL-3.0-or-later.html)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/leotaku/flycheck-aspell/check?logo=none&style=flat)](https://github.com/leotaku/flycheck-aspell/actions)
[![flycheck-aspell on MELPA](https://melpa.org/packages/flycheck-aspell-badge.svg)](https://melpa.org/#/flycheck-aspell)

## Basic usage

For natively supported modes, register your preferred checkers with Flycheck and then start `flycheck-mode` in the buffer you would like to spell-check.

```emacs-lisp
;; Ensure `flycheck-aspell' is available
(require 'flycheck-aspell)
;; If you want to check TeX/LaTeX/ConTeXt buffers
(add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
;; If you want to check Markdown/GFM buffers
(add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
;; If you want to check HTML buffers
(add-to-list 'flycheck-checkers 'html-aspell-dynamic)
;; If you want to check XML/SGML buffers
(add-to-list 'flycheck-checkers 'xml-aspell-dynamic)
;; If you want to check Nroff/Troff/Groff buffers
(add-to-list 'flycheck-checkers 'nroff-aspell-dynamic)
;; If you want to check Texinfo buffers
(add-to-list 'flycheck-checkers 'texinfo-aspell-dynamic)
;; If you want to check message buffers
(add-to-list 'flycheck-checkers 'mail-aspell-dynamic)
```

If you want to enable spell checking for a mode that is not in the above list, you should define and register an additional checker for that mode as seen below.

``` emacs-lisp
;; Because Aspell does not support Org syntax, the user has
;; to define a checker with the desired flags themselves.
(flycheck-aspell-define-checker "org"
  "Org" ("--add-filter" "url")
  (org-mode))
(add-to-list 'flycheck-checkers 'org-aspell-dynamic)
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
