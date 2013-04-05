;; functional.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile (require 'cl))


(defun func:foldr (f init seq)
  (loop
    for i from (1- (length seq)) downto 0 do
       (setq init (funcall f (elt seq i) init))
    finally return init))

(defun func:foldr1 (f xs)
  (let ((len (length xs)))
    (if (< len 2)
      (error "func:foldr1: sequence %S is too short (need at least 2 elements)")
      (func:foldr f (elt xs (1- len)) (subseq xs 0 (1- len))))))

(defun func:foldl (f init seq)
  (let ((len (1- (length seq))))
    (loop
      for i from 0 to len do
         (setq init (funcall f init (elt seq i)))
      finally return init)))

(defun func:foldl1 (f xs)
  (let ((len (length xs)))
    (if (< len 2)
      (error "func:foldl1: sequence %S is too short (need at least 2 elements)")
      (func:foldr f (elt xs (1- len)) (subseq xs 0 (1- len))))))


(defsubst func:all (pred seq)
  (func:foldr (lambda (x y) (and (funcall pred x) y)) t seq))

(defsubst func:any (pred seq)
  (func:foldr (lambda (x y) (or (funcall pred x) y)) nil seq))

(defun func:zip (xs ys)
  (let ((len (min (length xs) (length ys))))
    (do* ((i len (1- i))
          (z nil (cons (cons (elt xs i) (elt ys i)) z)))
         ((eql i 0) z))))

(defsubst func:concat (xs ys)
  (append xs ys))

(defsubst func:intercalate (elem xs)
  (when xs
    (cdr
     (func:foldr (lambda (y ys) (cons elem (cons y ys))) () xs))))

(defsubst func:intersperse (xs xss)
  (func:foldr #'func:concat nil (func:intercalate xs xss)))

(defun list-to-string (xs)
  (if (func:all #'characterp xs)
    (apply #'string xs)
    (error "list-to-string: invalid list: %S" xs)))

(defun string:ends-with (needle haystack)
  "Return t if HAYSTACK ends with NEEDLE"
  (when (<= (length needle)
            (length haystack))
    (func:all (lambda (x) (eql (car x) (cdr x)))
              (func:zip
               (reverse (string-to-list needle))
               (reverse (string-to-list haystack))))))

(defsubst string:intercalate (string strings)
  (apply #'string (func:intersperse string strings)))

(defsubst string:surround (string begin &optional end)
  (if end
    (concat begin string end)
    (concat begin string end)))

(provide 'functional)

;; Local Variables:
;; End:

;; functional.el ends here
