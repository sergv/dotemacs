;; all-lisp-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


;;;

(eval-when-compile (require 'cl-lib))

;;; old lisps

(defconst +lisp-modes+
  '(emacs-lisp-mode
    lisp-mode)
  "List of modes that are considered to be lisp.")

(autoload 'lisp-setup "general-lisp-setup")
(autoload 'lisp-repl-setup "general-lisp-setup")

(autoload 'emacs-lisp-setup "emacs-lisp-setup")
(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-setup)

(autoload 'lisp-pos-is-beginning-of-sexp? "general-lisp-setup")
(autoload 'lisp-pos-is-end-of-sexp? "general-lisp-setup")

(provide 'all-lisp-setup)

;; Local Variables:
;; End:

;; all-lisp-setup.el ends here
