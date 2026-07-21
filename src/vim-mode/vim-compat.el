;; vim-compat.el - Layer for interfacing different Emacsen --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

;; Check emacs and xemacs

(eval-when-compile
  (require 'cl-lib))

(require 'vim-macs)
(require 'vim-defs)

(defconst vim--ESC-event 'escape)

(defun vim-intercept-ESC ()
  "Waits a short time for further keys, otherwise sending [escape]."
  (interactive)
  (if (sit-for vim-intercept-ESC-timeout t)
    (push vim--ESC-event unread-command-events)
    (progn
      (add-hook 'pre-command-hook 'vim-enable-intercept-ESC)
      (vim-intercept-ESC-mode -1)
      (push last-command-event unread-command-events))))

(defmacro vim-called-interactively-p ()
  "Returns t iff the containing function has been called interactively."
  ;; TODO: perhaps (interactive-p) is enough?
  (if (fboundp 'called-interactively-p)
    '(called-interactively-p 'any)
    '(interactive-p)))

(defun vim--looking-back (regexp &optional limit greedy)
  "Return non-nil if text before point matches regular expression REGEXP.
Like `looking-at' except matches before point, and is slower.
LIMIT if non-nil speeds up the search by specifying a minimum
starting position, to avoid checking matches that would start
before LIMIT.
If GREEDY is non-nil, extend the match backwards as far as possible,
stopping when a single additional previous character cannot be part
of a match for REGEXP."
  (looking-back regexp limit greedy))

(defun vim-initialize-keymaps (enable)
  "Initialize keymaps when vim-mode is enabled."
  (if enable
    (add-to-list 'emulation-mode-map-alists 'vim-emulation-mode-alist t)
    (setq emulation-mode-map-alists
          (delq 'vim-emulation-mode-alist emulation-mode-map-alists))))

(provide 'vim-compat)

;; Local Variables:
;; End:

;; vim-compat.el ends here
