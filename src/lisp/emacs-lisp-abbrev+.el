;; emacs-lisp-abbrev+.el --- -*- lexical-binding: t; -*-

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
    emacs-lisp-print-info-skeleton
  :doc "Call to message to print some variables and messages."
  :print-begin "(message "
  :use-upcase nil

  :format-print-value "%s"
  :format-string-start "\""
  :format-string-end "\""

  :make-variable-list (lambda (list)
                        (mapconcat (lambda (var-name)
                                     (concat "(pp-to-string " var-name ")"))
                                   list
                                   "\n")))


(defun emacs-lisp-abbrev+-setup ()
  (setf abbrev+-skip-syntax '("w_" "w_(" ;;"^ >"
                              )
        abbrev+-abbreviations
        (list
         (list (make-abbrev+-re-for-lisp-func-name
                '(("beginning" 3)
                  ("of"        1)
                  ("line"      1)))
               "(beginning-of-line")
         (list (make-abbrev+-re-for-lisp-func-name
                '(("buffer"    3)
                  ("substring" 3)))
               "(buffer-substring")
         (list (make-abbrev+-re-for-lisp-func-name
                '(("buffer"     3)
                  ("substring"  3)
                  ("no"         1)
                  ("properties" 0)))
               "(buffer-substring-no-properties")
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
         (list "(\\(?:mess?\\|msg\\)\\_>"
               "(message")
         (list (make-abbrev+-re-for-lisp-func-name
                '(("condition" 3)
                  ("case"      1)))
               "(condition-case")
         (list (make-abbrev+-re-for-lisp-func-name
                '(("unwind"  2)
                  ("protecs" 2)))
               "(unwind-protect")
         (list "\\_<info\\_>"
               (list #'emacs-lisp-print-info-skeleton)
               (lambda () (and (not (lisp-point-inside-string-or-comment?))
                               (not (lisp-pos-is-beginning-of-list? (point))))))))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))


(provide 'emacs-lisp-abbrev+)

;; Local Variables:
;; End:

;; emacs-lisp-abbrev+.el ends here
