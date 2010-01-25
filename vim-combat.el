;;; vim-combat.el - Layer for interfacing different Emacsen

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(provide 'vim-combat)

;; Check emacs and xemacs

(defconst vim:xemacs-p (string-match "XEmacs" emacs-version))
(defconst vim:emacs-p (not vim:xemacs-p))

(cond 
 (vim:emacs-p
  
  (defun vim:set-keymap-default-binding (keymap command)
    "Sets the default binding of a keymap."
    (define-key keymap t command))

  (defun vim:called-interactively-p ()
    "Returns t iff the containing function has been called interactively."
    (called-interactively-p))

  )
 
 
 (vim:xemacs-p

  (defun vim:set-keymap-default-binding (keymap command)
    "Sets the default binding of a keymap."
    (set-keymap-default-binding keymap command))

  (defun vim:called-interactively-p ()
    "Returns t iff the containing function has been called interactively."
    (interactive-p))
  ))


;;; vim-combat.el ends here
