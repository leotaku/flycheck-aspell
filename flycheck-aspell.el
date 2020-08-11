;;; flycheck-aspell.el --- Aspell checker for flycheck -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2019 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 26 May 2019
;; Homepage: https://github.com/leotaku/flycheck-aspell
;; Keywords: flycheck, spell, aspell
;; Package-Version: 0.2.0
;; Package-Requires: ((flycheck "28.0") (emacs "25.1"))

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

;;; Commentary:

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

;; For seamless Ispell integration, I recommend setting the following variables:

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
;; + [X] honor Ispell localwords (they are marked as info)

;; * bottom footer :code:

(require 'flycheck)
(require 'ispell)

;;; Code:

(defgroup flycheck-aspell nil
  "Aspell checker for flycheck."
  :group 'flycheck
  :prefix "flycheck-aspell-")

(defmacro flycheck-aspell-define-checker (ft ft-doc flags modes)
  (let ((symbol (intern (concat ft "-aspell-dynamic"))))
    `(prog1
         (flycheck-define-checker ,symbol
           ,(format "A spell checker for %s files using aspell." ft-doc)
           :command ("aspell" "pipe" ,@flags)
           :error-parser flycheck-aspell--parse
           :modes ,modes)
       (setf (flycheck-checker-get ',symbol 'start)
             (lambda (checker callback)
               (let ((process (flycheck-start-command-checker checker callback))
                     (old-buffer (current-buffer)))
                 (with-temp-buffer
                   (insert-buffer-substring old-buffer)
                   (setf (point) (point-min))
                   (while (not (eobp))
                     (setf (point) (point-at-bol))
                     (insert "^")
                     (next-line))
                   (flycheck-process-send-buffer process))))))))
(put 'flycheck-aspell-define-checker 'lisp-indent-function 'defun)

(flycheck-aspell-define-checker "tex"
  "TeX" ("--add-filter" "url" "--add-filter" "tex") (tex-mode latex-mode context-mode))
(flycheck-aspell-define-checker "markdown"
  "Markdown" ("--add-filter" "url" "--add-filter" "html") (markdown-mode gfm-mode))
(flycheck-aspell-define-checker "html"
  "HTML" ("--add-filter" "url" "--add-filter" "html") (html-mode))
(flycheck-aspell-define-checker "nroff"
  "nroff/troff/groff" ("--add-filter" "url" "--add-filter" "nroff") (nroff-mode))
(flycheck-aspell-define-checker "texinfo"
  "Texinfo" ("--add-filter" "url" "--add-filter" "texinfo") (texinfo-mode))
(flycheck-aspell-define-checker "mail"
  "Mail" ("--add-filter" "url" "--add-filter" "email") (message-mode))

(defun flycheck-aspell--parse (output checker buffer)
  (let ((final-return nil)
        (errors (flycheck-aspell--process-text output)))
    (dolist (err errors)
      (when err
        (push
         (flycheck-aspell--format-error err checker buffer)
         final-return)))
    final-return))

(defun flycheck-aspell--format-error (err checker buffer)
  (when err
    (let ((line-number (nth 0 err))
          (column (nth 1 err))
          (word (nth 2 err))
          (suggestions (nth 3 err)))
      (flycheck-error-new-at
       line-number (1+ column)
	   (if (member word ispell-buffer-session-localwords)
		   'info 'error)
       (if (null suggestions)
    	   (concat "Unknown: " word)
         (concat "Suggest: " word " -> " suggestions))
       :checker checker
       :buffer buffer
       :filename (buffer-file-name buffer)))))

(defun flycheck-aspell--process-text (text)
  (let ((line-number 1)
        (errors '()))
    (dolist (line (split-string text "\n"))
      (if (= 0 (length line))
          (cl-incf line-number)
        (pcase (substring line 0 1)
          ("&" (progn
                 (push (cons line-number (flycheck-aspell--handle-and line)) errors)))
          ("#" (progn
                 (push (cons line-number (flycheck-aspell--handle-hash line)) errors)))
          ("*" nil)
          ("@" nil)
          (_ (error "Unknown beginning of line character in line %s" line)))))
    (nreverse errors)))

(defun flycheck-aspell--handle-hash (line)
  (string-match
   (rx line-start "# "			; start
       (group (+ wordchar)) " "	; error
       (group (+ digit)))		; column
   line)
  (let ((word (match-string 1 line))
	    (column (match-string 2 line)))
    (list (string-to-number column) word nil)))

(defun flycheck-aspell--handle-and (line)
  (string-match
   (rx line-start "& "			; start
       (group (+ wordchar)) " "	; error
       (+ digit) " "			; suggestion count
       (group (+ digit)) ": "	; column
       (group (+? anything)) line-end)
   line)
  (let ((word (match-string 1 line))
	    (column (match-string 2 line))
	    (suggestions (match-string 3 line)))
    (list (string-to-number column) word suggestions)))

(provide 'flycheck-aspell)

;;; flycheck-aspell.el ends here
