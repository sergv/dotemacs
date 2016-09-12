;; vim-compat.el - Layer for interfacing different Emacsen --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

;; Check emacs and xemacs

(eval-when-compile (require 'cl-lib))

(require 'vim-macs)
(require 'vim-defs)

(defconst vim:xemacs-p (string-match "XEmacs" emacs-version))
(defconst vim:emacs23-p (>= emacs-major-version 23))
(defconst vim:emacs-p (not vim:xemacs-p))

(defconst vim:default-region-face (if vim:xemacs-p 'zmacs-region 'region))
(defconst vim:deactivate-region-hook (if vim:xemacs-p
                                       'zmacs-deactivate-region-hook
                                       'deactivate-mark-hook))

(defmacro vim:emacsen (&rest impls)
  "Defines some body depending in emacs version."
  (while (and impls (not (eval (caar impls))))
    (pop impls))
  (if impls `(progn ,@(cdar impls))
      (error "Not implemented for this Emacs version")))

(defun vim:set-cursor (cursor)
  "Changes the cursor to type `cursor'."
  (setq cursor-type cursor))


(defun vim:set-keymap-default-binding (keymap command)
  "Sets the default binding of a keymap."
  (define-key keymap [t] command))

(defconst vim:ESC-event 'escape)

(defun vim:intercept-ESC ()
  "Waits a short time for further keys, otherwise sending [escape]."
  (interactive)
  (if (sit-for vim:intercept-ESC-timeout t)
    (push vim:ESC-event unread-command-events)
    (progn
      (add-hook 'pre-command-hook 'vim:enable-intercept-ESC)
      (vim:intercept-ESC-mode -1)
      (push last-command-event unread-command-events))))

(defmacro vim:called-interactively-p ()
  "Returns t iff the containing function has been called interactively."
  ;; TODO: perhaps (interactive-p) is enough?
  (if (fboundp 'called-interactively-p)
    '(called-interactively-p 'any)
    '(interactive-p)))

(defsubst vim:do-deactivate-mark () deactivate-mark)

(defalias 'vim:char-p 'integerp)


(if (fboundp 'match-substitute-replacement)
  (defalias 'vim:match-substitute-replacement 'match-substitute-replacement)
  ;; A simple definition I found somewhere in the web.
  (defun vim:match-substitute-replacement (replacement
                                           &optional fixedcase literal string subexp)
    "Return REPLACEMENT as it will be inserted by `replace-match'.
In other words, all back-references in the form `\\&' and `\\N'
are substituted with actual strings matched by the last search.
Optional FIXEDCASE, LITERAL, STRING and SUBEXP have the same
meaning as for `replace-match'."
    (let ((match (match-string 0 string)))
      (save-match-data
        (set-match-data (-map (lambda (x)
                                (if (numberp x)
                                  (- x (match-beginning 0))
                                  x))
                              (match-data t)))
        (replace-match replacement fixedcase literal match subexp)))))


(defun vim:looking-back (regexp &optional limit greedy)
  "Return non-nil if text before point matches regular expression REGEXP.
Like `looking-at' except matches before point, and is slower.
LIMIT if non-nil speeds up the search by specifying a minimum
starting position, to avoid checking matches that would start
before LIMIT.
If GREEDY is non-nil, extend the match backwards as far as possible,
stopping when a single additional previous character cannot be part
of a match for REGEXP."
  (looking-back regexp limit greedy))

(defun vim:initialize-keymaps (enable)
  "Initialize keymaps when vim-mode is enabled."
  (if enable
    (add-to-list 'emulation-mode-map-alists 'vim:emulation-mode-alist t)
    (setq emulation-mode-map-alists
          (delq 'vim:emulation-mode-alist emulation-mode-map-alists))))

(provide 'vim-compat)

;; Local Variables:
;; End:

;; vim-compat.el ends here
