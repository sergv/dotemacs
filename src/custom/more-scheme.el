;; more-scheme.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 10 July 2012
;; Description:

(require 'custom-predicates)

(defun string->symbol (str)
  (intern str))

(defun symbol->string (sym)
  (symbol-name sym))

(defun char->string (char)
  (char-to-string char))

(defun string->char (str)
  (string-to-char str))

(defun string->list (str)
  (coerce str 'list))

(defmacro begin (&rest body)
  `(progn ,@body))

(defconst else t)

(defun for-each (func items)
  (dolist (item items)
    (funcall func item)))

(provide 'more-scheme)

;; Local Variables:
;; lexical-binding: t
;; End:

;; more-scheme.el ends here
