;; shell-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 19 September 2013
;; Description:

(require 'macro-util)
(require 'abbrev+)

(define-print-info-skeleton
    shell-script-info-message-skeleton
  :doc "Call echo to show current some messages/values of specified variables."
  :print-begin "echo "
  :print-end ""

  :indent-after-func nil
  :insert-newline-before-var-list nil
  :msg-transform nil

  :format-print-value (lambda (msg) (concat "${" msg "}"))
  :format-string-start "\""
  :format-string-end "\""

  :insert-entity-name-procedure
  (constantly nil)

  :make-variable-list
  (constantly nil))


(defun shell-script-abbrev+-setup ()
  (setf abbrev+-skip-syntax '("w_" "w_(" ;;"^ >"
                              )
        abbrev+-abbreviations
        (list
         (list "\\_<info\\_>"
               (list #'shell-script-info-message-skeleton)
               (lambda () (and (not (lisp-point-inside-string-or-comment?))
                          (not (lisp-prev-pos-is-beginning-of-list? (point))))))))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'shell-script-abbrev+)

;; Local Variables:
;; End:

;; shell-abbrev+.el ends here
