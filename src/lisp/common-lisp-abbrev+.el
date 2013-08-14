;; common-lisp-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 19 December 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'general-lisp-setup)
(require 'abbrev+)

(define-lisp-print-info-skeleton
    common-lisp-print-info-skeleton
  :doc "Call to format to print some variables and messages."
  :print-begin "(format t "
  :use-upcase t

  :format-print-value "~A"
  :format-string-start "\"~&"
  :format-string-end "~%\""

  :make-variable-list (lambda (list)
                        (mapconcat #'identity list "\n")))


(defun common-lisp-abbrev+-setup ()
  ;; (add-hook 'abbrev+-after-expand-and-space-hook #'slime-echo-arglist nil t)
  (setf abbrev+-skip-syntax '("w_" "w_(" ;; "^ >"
                              )
        abbrev+-abbreviations
        (list
         (list (make-abbrev+-re-for-lisp-func-name
                '(("multiple" 1)
                  ("value"    1)
                  ("bind"     1)))
               "(multiple-value-bind")
         (list (make-abbrev+-re-for-lisp-func-name
                '(("destructuring" 3)
                  ("bind"          1)))
               "(destructuring-bind")
         (list (make-abbrev+-re-for-lisp-func-name
                '(("lambda" 3)))
               "(lambda")
         (list (make-abbrev+-re-for-lisp-func-name
                '(("unwind"  2)
                  ("protecs" 2)))
               "(unwind-protect")
         (list "\\_<info\\_>"
               (list #'common-lisp-print-info-skeleton)
               (lambda () (and (not (lisp-point-inside-string-or-comment?))
                               (not (lisp-pos-is-beginning-of-list? (point))))))))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))


(provide 'common-lisp-abbrev+)

;; Local Variables:
;; End:

;; emacs-lisp-abbrev+.el ends here
