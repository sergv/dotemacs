;; lisp-autoloads.el --- -*- lexical-binding: t; -*-

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
    clojure-mode
    lisp-mode)
  "List of modes that are considered to be lisp.")

(autoload 'lisp-setup "general-lisp-setup")
(autoload 'lisp-repl-setup "general-lisp-setup")
(add-hook 'lisp-mode-hook #'lisp-setup)

(autoload 'emacs-lisp-setup "emacs-lisp-setup")
(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-setup)

(autoload 'lisp-pos-is-beginning-of-sexp? "general-lisp-setup")
(autoload 'lisp-pos-is-end-of-sexp? "general-lisp-setup")

(require 'clojure-mode-autoloads)

(autoload 'clojure-setup "clojure-setup")
(add-hook 'clojure-mode-hook #'clojure-setup)

(provide 'lisp-autoloads)

;; Local Variables:
;; End:

;; lisp-autoloads.el ends here
