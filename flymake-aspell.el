;;; flymake-aspell.el --- Aspell checker for flymake -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2019 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 26 May 2019
;; Homepage: https://github.com/leotaku/flycheck-aspell
;; Keywords: flymake, spell, aspell
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

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
;;

;; This file provides a flymake spell-checker for documents using
;; "aspell" as a backend.  You must be running Emacs 26 or newer.
;; Enable it by adding the following to your init file:

;;   (add-hook 'text-mode-hook #'flymake-aspell-setup)

(require 'mode-local)
(require 'pcase)
(require 'rx)
(require 'ispell)
(eval-when-compile
  (require 'cl-lib))

;;; Code:

(defvar flymake-aspell-additional-ispell-integration t
  "By default, flymake-aspell advices some Ispell functions to immediately
unmark accepted words.
This behavior can be disabled by setting this option to nil.")

(defvar-local flymake-aspell-aspell-mode "url"
  "Aspell mode that should be used by the spell checker.

The flymake-aspell package tries to provide sensible defaults for
all modes natively supported by aspell and some additional ones
the author of this package regularly uses.

If your favorite major-mode is not supported, please run
\"aspell dump modes\" in your terminal and add the fitting mode
to your major-mode by using `setq-mode-local'.

NOTE: aspell also supports a powerful way to describe \"custom modes\"
using its \"context\" filter.
This is not currently supported directly through flymake-aspell, but
you can manually set any option using the `flyspell-aspell-local-aspell-options'
variable.
")

(defvar-local flymake-aspell-local-aspell-options '()
  "List of additional local options for the flymake-aspell aspell process.")

(defvar-local flymake-aspell-aspell-options '("--guess" "--sug-mode=normal")
  "List of additional options for the flymake-aspell aspell process.")

(defvar-local flymake-aspell--proc nil
  "A buffer-local variable handling the aspell process for flymake.")

(cl-defmacro flymake-aspell-set-modes (&rest pairs)
  `(progn
     ,@(mapcar
        (lambda (pair)
          `(setq-mode-local ,(car pair) flymake-aspell-aspell-mode ,(cadr pair)))
        pairs)))

(flymake-aspell-set-modes
 (sgml-mode "sgml")
 (xml-mode "sgml")
 (c++-mode "ccpp")
 (c-mode "ccpp")
 (rust-mode "ccpp")
 (nix-mode "comment")
 (tex-mode "tex")
 (latex-mode "tex")
 (context-mode "tex")
 (perl-mode "perl")
 (python-mode "perl")
 (html-mode "html")
 (message-mode "email")
 (nroff-mode "nroff")
 (texinfo-mode "texinfo"))

(defun flymake-aspell--check (report-fn &rest _args)
  "Run flymake spell checker.

REPORT-FN is flymake's callback function."
  (unless (executable-find "aspell")
    (error "Cannot find aspell"))
  (when (process-live-p flymake-aspell--proc)
    (kill-process flymake-aspell--proc))
  (let ((source (current-buffer))
        (dict (if (featurep 'ispell)
                  (or ispell-local-dictionary
                      ispell-dictionary)
                "en_US"))
        (mode flymake-aspell-aspell-mode))
    (save-restriction
      (widen)
      (setq
       flymake-aspell--proc
       (make-process
        :name "flymake-aspell" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *flymake-aspell*")
        :command `("aspell" "pipe" "-d" ,dict "--mode" ,mode
                   ,@flymake-aspell-aspell-options ,@flymake-aspell-local-aspell-options)
        :sentinel
        (lambda (proc _event)
          (when (eq 'exit (process-status proc))
            (unwind-protect
                (with-current-buffer source
                  (if (eq proc flymake-aspell--proc)
                      (let ((errors (flymake-aspell--process-text
                                     (with-current-buffer (process-buffer proc)
                                       (buffer-string)))))
                        (save-excursion
                          (goto-char (point-min))
                          (cl-loop
                           for (line word column suggestions) in errors
                           for (beg . end) = (condition-case nil
                                                 (progn
                                                   (forward-line line)
                                                   (let ((pos (+ (point) (1- column))))
                                                     (cons pos (+ pos (length word)))))
                                               (error (cons 0 0)))
                           collect (flymake-make-diagnostic
                                    source
                                    beg
                                    end
                                    :error
                                    (format "%s: %s" word suggestions))
                           into diags
                           finally (funcall report-fn diags))))
                    (flymake-log :warning "Canceling obsolete check %s"
                                 proc)))
              (kill-buffer (process-buffer proc)))))))
      ;; enter terse mode for better performance
      (process-send-string flymake-aspell--proc "!\n")
      (dolist (line (split-string (buffer-string) "\n"))
        (process-send-string flymake-aspell--proc (concat "^" line "\n")))
      (process-send-eof flymake-aspell--proc))))

(defun flymake-aspell-setup ()
  "Enable the spell checker for the current buffer."
  (interactive)
  (unless (memq 'flymake-aspell--check flymake-diagnostic-functions)
    (make-local-variable 'flymake-diagnostic-functions)
    (push 'flymake-aspell--check flymake-diagnostic-functions)))

(defun flymake-aspell-maybe-recheck (&rest _)
  (when (bound-and-true-p flymake-mode)
    (flymake-start)))

(when flymake-aspell-additional-ispell-integration
  (advice-add 'ispell-pdict-save :after 'flymake-aspell-maybe-recheck)
  (advice-add 'ispell-change-dictionary :after 'flymake-aspell-maybe-recheck))

(defun flymake-aspell--process-text (text)
  (let ((line-number 0)
        (errors '()))
    (dolist (line (split-string text "\n"))
      (if (= 0 (length line))
          (cl-incf line-number)
        (pcase (substring line 0 1)
          ("&" (progn
                 (push (cons line-number (flymake-aspell--handle-and line)) errors)
                 (setq line-number 0)))
          ("#" (progn
                 (push (cons line-number (flymake-aspell--handle-hash line)) errors)
                 (setq line-number 0)))
          ("*" nil)
          ("@" nil)
          (_ (error "Unknown beginning of line character in line %s"
                    line)))))
    (nreverse errors)))

(defun flymake-aspell--handle-hash (line)
  "Handle a LINE of aspell output starting with a hash (#) sign."
  (string-match
   (rx line-start "# "                  ; start
       (group (+ wordchar)) " "         ; error
       (group (+ digit)))               ; column
   line)
  (let ((word (match-string 1 line))
	    (column (match-string 2 line)))
    (list word (string-to-number column) nil)))

(defun flymake-aspell--handle-and (line)
  "Handle a LINE of aspell output starting with an and (&) sign."
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
    (list word (string-to-number column) suggestions)))

(provide 'flymake-aspell)

;;; flymake-aspell.el ends here
