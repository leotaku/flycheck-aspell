;;; flycheck-aspell --- simple spell check using flycheck and aspell -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2019 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 26 May 2019
;; Homepage: https://github.com/leotaku/flycheck-aspell
;; Keywords: flycheck, spell, aspell
;; Package-Version: 0.1.0
;; Package-Requires: ((flycheck "31") (emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary

;; * flycheck-aspell :README:

;; ** IMPORTANT FOR USERS

;; This repository now also contains the =flymake-aspell= package,
;; which is what I currently use for spellchecking.
;; Documentation regarding said package can be found in its =.el=
;; file.

;; The =flymake-aspell= package is more recent and written in a more
;; consistent style, which might make it faster and more featureful
;; than =flycheck-aspell=.

;; However =flycheck-aspell= should still remain usable for the
;; forseeable future.

;; ![[file:screenshot.png][flycheck-aspell in action]]

;; This package adds support for spell checking to flycheck using
;; the [[http://aspell.net][GNU aspell]] application.

;; It is a successor (and complete rewrite) to my
;; [[https://github.com/leotaku/flycheck-hunspell][flycheck-hunspell]]
;; project, which was crippled by the bad performance of hunspell when
;; used with larger files.
;; (aspell performs aproximately 30x faster in the cases I tested.)

;; Aspell also seems to be a bit more flexible than hunspell with regard
;; to filters, which might prove to be useful in the future.

;; ** Installation

;; I recommend using [[https://github.com/raxod502/straight.el][straight.el]] for
;; installing non-(m)elpa sources.

;; ** Usage

;; Simply register your preferred checkers with flycheck and then start =flycheck-mode=
;; in the buffer you would like to spell-check.
;; (see [[Features]] for supported filetypes)

;; #+begin_src elisp
;; (require 'flycheck-aspell)
;; (add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
;; #+end_src

;; The dictionary the checkers use is determined by the value of
;; `ispell-local-dictionary` or `ispell-dictionary`.

;; It might be wise to skim the [[https://www.flycheck.org/en/latest/][flycheck docs]]
;; to learn how to efficently use and configure flycheck.

;; You also need to install the GNU =aspell= and =sed= binaries.
;; =sed= is needed for preprocessing the file that is sent to aspell
;; with high performance.

;; All major linux distributions should package these and there are
;; probably working macports or something.

;; ** Configuration

;; For seamless ispell integration, I recommend setting the following variables:

;; #+begin_src elisp
;; (setq ispell-dictionary "some_dictionary"
;;       ispell-program-name "aspell"
;; 	  ispell-silently-savep t)
;; #+end_src

;; [[https://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html][This post]]
;; might also be of interest.

;; You may also want to advice `ispell-pdict-save` for instant feedback when inserting
;; new entries into your local dictionary:

;; #+begin_src elisp
;; (advice-add 'ispell-pdict-save :after 'flycheck-maybe-recheck)
;; (defun flycheck-maybe-recheck (_)
;;   (when (bound-and-true-p flycheck-mode)
;;    (flycheck-buffer))
;; #+end_src

;; ** TODO Features

;; + [X] initial featureset
;; + [X] checkers for all filters (all with url support)
;;   - [X] TeX
;;   - [X] markdown
;;   - [X] nroff
;;   - [X] html
;;   - [X] texinfo
;;   - [X] email (message-mode)
;; + [ ] tests
;; + [X] honor ispell localwords (they are marked as info)

;; * bottom footer :code:

(require 'flycheck)
(require 'ispell)

(defcustom flycheck-aspell-aspell-executable
  "aspell"
  "Path of the aspell executable used by all flycheck-aspell checkers.")

(defcustom flycheck-aspell-sed-executable
  "sed"
  "Path of the sed executable used by all flycheck-aspell checkers.")

(defmacro flycheck-aspell-define-checker (ft ft-doc flags modes)
  `(flycheck-define-checker ,(intern (concat ft "-aspell-dynamic"))
     ,(format "A spell checker for %s files using aspell." ft-doc)
     :command ("sh" "-c"
	       (eval
		(concat
		 flycheck-aspell-sed-executable " s/^/^/ | "
		 flycheck-aspell-aspell-executable " pipe -d "
		 (or ispell-local-dictionary
		     ispell-dictionary
		     "en_US")
		 " "
		 (mapconcat 'identity ',flags " "))))
     :standard-input t
     :error-parser flycheck-parse-aspell
     :modes ,modes))
(put 'flycheck-aspell-define-checker 'lisp-indent-function 'defun)

(flycheck-aspell-define-checker "tex"
  "TeX" ("--add-filter" "url" "--add-filter" "tex") (tex-mode latex-mode context-mode))
(flycheck-aspell-define-checker "markdown"
  "Markdown" ("--add-filter" "url" "--add-filter" "html") (markdown-mode))
(flycheck-aspell-define-checker "html"
  "HTML" ("--add-filter" "url" "--add-filter" "html") (html-mode))
(flycheck-aspell-define-checker "nroff"
  "nroff/troff/groff" ("--add-filter" "url" "--add-filter" "nroff") (nroff-mode))
(flycheck-aspell-define-checker "texinfo"
  "Texinfo" ("--add-filter" "url" "--add-filter" "texinfo") (texinfo-mode))
(flycheck-aspell-define-checker "mail"
  "Mail" ("--add-filter" "url" "--add-filter" "email") (message-mode))

(defun flycheck-parse-aspell (output checker buffer)
  (let ((final-return nil)
	(line-number 1)
	(buffer-lines
	 (split-string
	  (with-current-buffer buffer
	    (substring-no-properties (buffer-string)))
	  "\n"))
	(error-structs
	 (mapcar 'flycheck-aspell-handle-line
		 (split-string output "\n"))))
    (dolist (struct error-structs)
      (unless (null struct)
	(let* ((word (nth 0 struct))
    	       (column (nth 1 struct))
    	       (suggestions (nth 2 struct)))
    	  (while (not (or (null (cdr buffer-lines))
			  (string-match-p
			   (concat
			    "\\(?:\\<\\|[^[:alpha:]]\\)"
			    word
			    "\\(?:\\>\\|[^[:alpha:]]\\)")
			   (car buffer-lines))))
    	    (setq buffer-lines (cdr buffer-lines))
    	    (setq line-number (1+ line-number)))
	  (setf (car buffer-lines)
		(concat
		 (make-string (1+ column) ?.)
		 (substring (car buffer-lines) (1+ column))))
	  ;; (message "%s: %s" word (car buffer-lines))
	  (push
	   (flycheck-error-new-at
    	    line-number (1+ column)
	    (if (member word ispell-buffer-session-localwords)
		'info 'error)
    	    (if (null suggestions)
    		(concat "Unknown: " word)
    	      (concat "Suggest: " word " -> " suggestions))
    	    :checker checker
    	    :buffer buffer
    	    :filename (buffer-file-name buffer))
	   final-return))))
    final-return))

(defun flycheck-aspell-handle-line (line)
  (cond
   ;; # indicates that no replacement could be found
   ((string-match-p "^#" line)
    (flycheck-aspell-handle-hash line))
   ;; & indicates that replacements could be found
   ((string-match-p "^&" line)
    (flycheck-aspell-handle-and line))
   ;; other lines are irrelevant
   (t
    nil)))

(defun flycheck-aspell-handle-hash (line)
  (string-match
   (rx line-start "# "			; start
       (group (+ char)) " "		; error
       (group (+ digit)))		; column
   line)
  (let ((word (match-string 1 line))
	(column (match-string 2 line)))
    (list word (string-to-number column) nil)))

(defun flycheck-aspell-handle-and (line)
  (string-match
   (rx line-start "& "			; start
       (group (+ char)) " "		; error
       (+ digit) " "			; suggestion count
       (group (+ digit)) ": "		; column
       (group (+? anything)) line-end)
   line)
  (let ((word (match-string 1 line))
	(column (match-string 2 line))
	(suggestions (match-string 3 line)))
    (list word (string-to-number column) suggestions)))

(provide 'flycheck-aspell)
