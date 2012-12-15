;; blueprint-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 14 December 2012
;; Description:

(require 'general-lisp-setup)
(require 'slime-setup)

(define-common-lisp-style "blueprint"
  "My custom indent style for blueprint."
  (:indentation
   (if (4 2 2))
   (aif (as if))
   (begin (2 &body))
   (define (4 &rest 2))
   (cond (&rest (&whole 2 &body)))
   (set! nil)
   (let ((&whole 4 &rest (&whole 1 2 2)) &body))
   (let* (as let))
   (letrec (as let))))


(autoload 'blueprint-mode "blueprint-mode" "Major mode for Blueprint files" t)

(add-to-list 'auto-mode-alist
             '("\\.bp\\'" . blueprint-mode))

(defun blueprint-setup ()
  (lisp-setup :use-whitespace t)
  (common-lisp-set-style "blueprint")
  (setq-local paredit-space-for-delimiter-predicates
              (list (lambda (end? delim)
                      ;; never insert spaces after # is typed
                      nil)))
  (setf lisp-indent-function #'common-lisp-indent-function))

(add-hook 'blueprint-mode-hook #'blueprint-setup)

(provide 'blueprint-setup)

;; Local Variables:
;; End:

;; blueprint-setup.el ends here
