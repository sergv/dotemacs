;; datastructures.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  5 March 2013
;; Description:

(eval-when-compile (require 'cl-lib))

;;;; persistent sorted sets

(defstruct sorted-set
  items
  lt-pred)

(defun sorted-set/add (set item)
  "Return new sorted set with ITEM added."
  (let* ((res-items (cons nil nil))
         (tmp res-items)
         (items (sorted-set-items set)))
    (while (and items
                (funcall (sorted-set-lt-pred set) (first items) item))
      (setf (rest tmp) (cons (first items) nil)
            tmp (rest tmp)
            items (rest items)))
    (setf (rest tmp) (cons item items))
    (make-sorted-set :items (rest res-items)
                     :lt-pred (sorted-set-lt-pred set))))

(defun sorted-set/intersection (set1 set2)
  "Intersect two sets, SET1 and SET2 whose lt-than predicates must be consistest.
Item equality is determined using LT-THAN:
X ~ Y == (and (not (lt-than X Y)) (not (lt-than Y X)))."
  (let* ((result (cons nil nil))
         (tmp result)
         (lt-than (sorted-set-lt-pred set1))
         (items1 (sorted-set-items set1))
         (items2 (sorted-set-items set2)))
    (while (and items1 items2)
      (cond ((funcall lt-than (first items1) (first items2))
             (setf items1 (rest items1)))
            ((funcall lt-than (first items2) (first items1))
             (setf items2 (rest items2)))
            (else
             (setf (rest tmp) (cons (first items1) nil)
                   tmp (rest tmp)
                   items1 (rest items1)
                   items2 (rest items2)))))
    (make-sorted-set :items (rest result)
                     :lt-pred lt-than)))


(defun sorted-set/from-list (items lt-pred)
  "Construct sorted set from ITEMS list using LT-PRED predicate to sotr
items and remove any duplicates."
  (let ((remove-duplicates
         (lambda (items)
           ;; items is sotred here
           (let* ((result (cons (first items) nil))
                  (tmp result))
             (setf items (rest items))
             (while items
               (if (funcall lt-pred (first tmp) (first items))
                 (setf (rest tmp) (cons (first items) nil)
                       tmp (rest tmp)))
               (setf items (rest items)))
             result))))
    (make-sorted-set :items (remove-duplicates (sort (copy-list items) lt-pred))
                     :lt-pred lt-pred)))

(defun sorted-set/empty (lt-pred)
  "Construct empty set with given predicate, LT-PRED ."
  (make-sorted-set :items '() :lt-pred lt-pred))

(defun sorted-set/empty? (set)
  "Check whether SET is empty."
  (null? (sorted-set-items set)))

(defun sorted-set/length (set)
  "Get number of items in SET."
  (length (sorted-set-items set)))

(provide 'datastructures)

;; Local Variables:
;; End:

;; datastructures.el ends here
