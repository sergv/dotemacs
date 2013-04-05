;; more-scheme.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 10 July 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'custom-predicates)

(defun string->symbol (str)
  (intern str))

(defun symbol->string (sym)
  (symbol-name sym))

(defun char->string (char)
  (char-to-string char))

(defun string->char (str)
  (string-to-char str))


(defun number->string (n)
  (number-to-string n))

(defun string->number (str)
  (string-to-number str))


(defun string->list (str)
  (coerce str 'list))

(defun list->string (items)
  (coerce items 'string))

(defun vector->vector (str)
  (coerce str 'list))

(defun list->vector (items)
  (coerce items 'vector))


(defun char=? (a b)
  (char-equal a b))

(defmacro begin (&rest body)
  `(progn ,@body))

(defconst else t)

(defun for-each (func items)
  (dolist (item items)
    (funcall func item)))


(defun any? (pred items)
  (funcall #'some pred items))

(defun all? (pred items)
  (funcall #'every pred items))


(defun list-ref (i list)
  (nth i list))

(defun vector-ref (i vect)
  (aref i vect))


(provide 'more-scheme)

;; Local Variables:
;; End:

;; more-scheme.el ends here
