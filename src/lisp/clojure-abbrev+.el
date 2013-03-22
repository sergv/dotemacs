;; clojure-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  3 February 2013
;; Description:

(require 'abbrev+)

(define-lisp-print-info-skeleton
    clojure-print-info-skeleton
  :doc "Insert to printf to print some variables and messages."
  :print-begin "(print (format "
  :use-upcase nil

  :format-print-value "%s"
  :format-string-start "\""
  :format-string-end "\\n\""
  :print-end "))"

  :make-variable-list #'join-lines)

(defun clojure-abbrev+-setup ()
  (setf abbrev+-skip-syntax '("w_" "w_(" ;;"^ >"
                              )
        abbrev+-abbreviations
        (list
         (list "\\_<info\\_>"
               (list #'clojure-print-info-skeleton)
               (lambda () (and (not (lisp-point-inside-string-or-comment?))
                               (not (lisp-pos-is-beginning-of-list? (point))))))))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'clojure-abbrev+)

;; Local Variables:
;; End:

;; clojure-abbrev+.el ends here
