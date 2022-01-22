;; append-list.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 20 January 2022
;; Description:


(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(defvar append-list-tag '#:append-list)
(defvar append-list-shared-tail-tag '#:shared-tail)

(defsubst append-list-p (x)
  (and (consp x)
       (eq append-list-tag (caar x))
       (eq append-list-shared-tail-tag (cadr x))))

(defsubst append-list-null (xs)
  (cl-assert (append-list-p xs) "Invalid append list: %s" xs)
  (cdar-sure xs))

(defsubst append-list-empty ()
  (let ((cell (cons append-list-tag nil)))
    (cons cell
          ;; Extra indirection to support ‘append-list-fork-tail’ operation.
          (cons append-list-shared-tail-tag cell))))

(defsubst append-list-singleton (item)
  (let ((cell (cons item nil)))
    (cons (cons append-list-tag cell)
          (cons append-list-shared-tail-tag cell))))

(defsubst append-list-append! (xs x)
  (cl-assert (append-list-p xs) "Invalid append list: %s" xs)
  (let ((shared-tl (cdr-sure xs)))
    (setcdr-sure shared-tl
                 (setcdr-sure (cdr-sure shared-tl)
                              (cons x nil)))))

(defun append-list-prepend! (xs x)
  (cl-assert (append-list-p xs) "Invalid append list: %s" xs)
  (let ((hd (car-sure xs)))
    (if (cdr-sure hd)
        (setcdr-sure hd (cons x
                              (cdr-sure hd)))
      (append-list-append! xs x))))

(defsubst append-list-fork-tail (xs)
  (cl-assert (append-list-p xs) "Invalid append list: %s" xs)
  (let ((hd        (car-sure xs))
        (shared-tl (cdr-sure xs)))
    (if (cdr-sure hd)
        (cons (cons append-list-tag
                    (cdr-sure shared-tl))
              shared-tl)
      (progn
        (cl-assert (eq hd (cdr shared-tl)))
        (cons (cdr-sure shared-tl)
              shared-tl)))))

(defsubst append-list-reify (xs)
  (cl-assert (append-list-p xs) "Invalid append list: %s" xs)
  (cdar-sure xs))

(defsubst append-list-extend-with! (xs ys)
  "Make sure XS ends with elements from YS. It’s *not specified* what happens if
YS is appended to after this operation. XS may or may not receive those elements."
  (cl-assert (append-list-p xs) "Invalid append list: %s" xs)
  (cl-assert (append-list-p ys) "Invalid append list: %s" ys)
  ;; Don’t do anything if secon list is empty
  (when-let (ys-contents  (cdar-sure ys))
    (let ((shared-tl-xs (cdr-sure xs))
          (shared-tl-ys (cdr-sure ys)))
      (cl-assert (eq append-list-shared-tail-tag (car shared-tl-xs)))
      (cl-assert (eq append-list-shared-tail-tag (car shared-tl-ys)))
      (setcdr-sure xs shared-tl-ys)
      (setcdr-sure (cdr-sure shared-tl-xs) ys-contents))))

(provide 'append-list)

;; Local Variables:
;; End:

;; append-list.el ends here
