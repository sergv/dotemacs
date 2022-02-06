;; common-small.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  3 August 2021
;; Description:

(when-emacs-version (or (< it 28)
                        (not (and (fboundp #'native-comp-available-p)
                                  (native-comp-available-p))))
  (defsubst comp-hint-cons (x)
    (cl-assert (and x (consp x)))
    x)
  (defsubst comp-hint-fixnum (x)
    (cl-assert (fixnump x))
    x))

(when-emacs-version (<= 28 it )
  ;; For comp-hint-cons and comp-hint-fixnum.
  (require 'comp))

(defsubst char= (a b)
  (declare (pure t) (side-effect-free t))
  (char-equal a b))

(defsubst caar-safe (x)
  (declare (pure t) (side-effect-free t))
  (car-safe (car-safe x)))

(defsubst cadr-safe (x)
  (declare (pure t) (side-effect-free t))
  (car-safe (cdr-safe x)))

(defsubst cddr-safe (x)
  (declare (pure t) (side-effect-free t))
  (cdr-safe (cdr-safe x)))

(defsubst cdar-safe (x)
  (declare (pure t) (side-effect-free t))
  (cdr-safe (car-safe x)))

;;;;

(defsubst string->symbol (str)
  "Convert string STR to symbol."
  (declare (pure nil) (side-effect-free nil))
  (intern str))

(defsubst symbol->string (sym)
  "Convert symbol SYM to string."
  (declare (pure t) (side-effect-free t))
  (symbol-name sym))

(defsubst char->string (char)
  (declare (pure t) (side-effect-free t))
  (char-to-string char))

(defsubst string->char (str)
  (declare (pure t) (side-effect-free t))
  (string-to-char str))


(defsubst number->string (n)
  (declare (pure t) (side-effect-free t))
  (number-to-string n))

(defsubst string->number (str)
  (declare (pure t) (side-effect-free t))
  (string-to-number str))


(defsubst string->list (str)
  (declare (pure t) (side-effect-free t))
  (append str nil))

(defsubst list->string (items)
  (declare (pure t) (side-effect-free t))
  (concat items))

(defsubst vector->list (x)
  (declare (pure t) (side-effect-free t))
  (append x nil))

(defsubst list->vector (items)
  (declare (pure t) (side-effect-free t))
  (vconcat items))

(defsubst int-vector->string (v)
  "Convernt vector of integers to string."
  (declare (pure t) (side-effect-free t))
  (concat v))

;; todo: remove
(defsubst char=? (a b)
  (declare (pure t) (side-effect-free t))
  (char-equal a b))

(provide 'common-small)

;; Local Variables:
;; End:

;; common-small.el ends here
