;; append-list.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 20 January 2022
;; Description:


(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(defvar append-list-tag '#:append-list-tag)

(defsubst append-list-p (x)
  (and (consp x)
       (eq append-list-tag (car (car-sure x)))))

(defsubst append-list-null (xs)
  (cl-assert (append-list-p xs) "Invalid append list: %s" xs)
  (null (cdar-sure xs)))

(defsubst append-list-empty ()
  (let ((cell (cons append-list-tag nil)))
    (cons cell cell)))

(defsubst append-list-singleton (item)
  (let ((cell (cons item nil)))
    (cons (cons append-list-tag cell) cell)))

(defsubst append-list-append! (xs x)
  (cl-assert (append-list-p xs) "Invalid append list: %s" xs)
  (setcdr-sure xs
                 (setcdr-sure (cdr-sure xs)
                              (cons x nil))))

(defun append-list-prepend! (xs x)
  (cl-assert (append-list-p xs) "Invalid append list: %s" xs)
  (let ((hd (car-sure xs)))
    (if (cdr-sure hd)
        (setcdr-sure hd (cons x
                              (cdr-sure hd)))
      (append-list-append! xs x))))

(defun append-list-get-last-cons (xs)
  (cl-assert (append-list-p xs) "Invalid append list: %s" xs)
  (when (append-list-null xs)
    (error "Cannot get last cons of an empty list"))
  (cdr-sure xs))

(defsubst append-list-reify (xs)
  (cl-assert (append-list-p xs) "Invalid append list: %s" xs)
  (cdar-sure xs))

(defsubst append-list-extend-with! (xs ys)
  "Make sure XS ends with elements from YS. It’s *not specified* what happens if
YS is appended to after this operation. XS may or may not receive those elements.

Semantically YS should be considered consumed here and best not used afterwards."
  (cl-assert (append-list-p xs) "Invalid append list: %s" xs)
  (cl-assert (append-list-p ys) "Invalid append list: %s" ys)
  ;; Don’t do anything if second list is empty
  (when-let (ys-contents (cdar-sure ys))
    (setcdr-sure (cdr-sure xs) ys-contents)
    (setcdr-sure xs (cdr-sure ys))))

(provide 'append-list)

;; Local Variables:
;; End:

;; append-list.el ends here
