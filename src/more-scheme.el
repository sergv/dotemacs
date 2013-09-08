;; more-scheme.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 10 July 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'custom-predicates)

(defsubst string->symbol (str)
  (intern str))

(defsubst symbol->string (sym)
  (symbol-name sym))

(defsubst char->string (char)
  (char-to-string char))

(defsubst string->char (str)
  (string-to-char str))


(defsubst number->string (n)
  (number-to-string n))

(defsubst string->number (str)
  (string-to-number str))


(defsubst string->list (str)
  (coerce str 'list))

(defsubst list->string (items)
  (coerce items 'string))

(defsubst vector->list (str)
  (coerce str 'list))

(defsubst list->vector (items)
  (coerce items 'vector))

(defsubst int-vector->string (v)
  "Convernt vector of integers to string."
  (coerce v 'string))

(defsubst char=? (a b)
  (char-equal a b))

(defmacro begin (&rest body)
  `(progn ,@body))

(defconst else t)

(defun for-each (func items)
  (dolist (item items)
    (funcall func item)))


(defun any? (pred items)
  "Returns t if pred returns t for any element of ITEMS."
  (funcall #'some pred items))

(defun all? (pred items)
  "Returns t if pred returns t for all elements of ITEMS."
  (funcall #'every pred items))


(defun list-ref (i list)
  (nth i list))

(defun vector-ref (i vect)
  (aref i vect))


(provide 'more-scheme)

;; Local Variables:
;; End:

;; more-scheme.el ends here
