;; all-lisp-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


;;;

(require 'cl)
;; (require 'cl-mode)

(defconst +scheme-file-extensions+
  '("scm" "sc" "stk" "ss" "sch" "oak")
  "List of scheme file extensions")

(setf auto-mode-alist
      (cons (cons (eval `(rx "." (or ,@+scheme-file-extensions+) eot))
                  'scheme-mode)
            (remove-if (lambda (x)
                         (memq x '(scheme-mode)))
                       auto-mode-alist
                       :key #'cdr)))


(autoload 'lisp-setup "general-lisp-setup")
(autoload 'lisp-repl-setup "general-lisp-setup")

(autoload 'scheme-setup "scheme-setup")
(autoload 'scheme-repl-setup "scheme-setup")

(add-hook 'scheme-mode-hook #'scheme-setup)
(add-hook 'inferior-scheme-mode-hook #'scheme-repl-setup)

(autoload 'emacs-lisp-setup "emacs-lisp-setup")
(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-setup)

(autoload 'lisp-pos-is-beginning-of-sexp? "general-lisp-setup")
(autoload 'lisp-pos-is-end-of-sexp? "general-lisp-setup")

(provide 'all-lisp-setup)

;; Local Variables:
;; End:

;; all-lisp-setup.el ends here
