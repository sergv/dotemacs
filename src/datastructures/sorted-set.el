;; sorted-set.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 16 January 2022
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

;;; (prematurely optimized) persistent sorted sets

(cl-defstruct (sorted-set
            (:conc-name sorted-set/))
  items
  lt-pred
  length)


(defun sorted-set/rest (set)
  "Produce new sorted set without minimal item."
  (make-sorted-set :items (cdr (sorted-set/items set))
                   :lt-pred (sorted-set/lt-pred set)
                   :length (- (sorted-set/length set) 1)))

;; yes, this really should construct new set every time
(defun sorted-set/add (set item)
  "Return new sorted set with ITEM added."
  (let* ((res-items (cons nil nil))
         (tmp res-items)
         (items (sorted-set/items set))
         (lt-pred (sorted-set/lt-pred set)))
    (while (and (not (null items))
                (funcall lt-pred (first items) item))
      (setf (rest tmp) (cons (first items) nil)
            tmp (rest tmp)
            items (rest items)))
    (if (and (not (null items))
             ;; If existing item is not greater thar the
             ;; one we're trying to insert then do not
             ;; insert it.
             (not (funcall lt-pred item (first items))))
      set
      (progn
        (setf (rest tmp) (cons item items))
        (make-sorted-set :items (rest res-items)
                         :lt-pred (sorted-set/lt-pred set)
                         :length (+ 1 (sorted-set/length set)))))))

(defun sorted-set/union (set1 set2)
  "Union two sets, SET1 and SET2 whose lt-than predicates must be consistest.
Item equality is determined using LT-THAN:
X ~ Y == (and (not (lt-than X Y)) (not (lt-than Y X)))."
  (cl-assert (equal? (sorted-set/lt-pred set1)
                     (sorted-set/lt-pred set2))
             nil
             "Cannot merge sets with different lt predicates")
  (let* ((result (cons nil nil))
         (tmp result)
         (lt-than (sorted-set/lt-pred set1))
         (items1 (sorted-set/items set1))
         (items2 (sorted-set/items set2))
         (len 0))
    (while (or (not (null items1))
               (not (null items2)))
      (cond ((null items1)
             (setf (rest tmp) items2
                   len (+ len (length items2))
                   items2 nil))
            ((null items2)
             (setf (rest tmp) items1
                   len (+ len (length items1))
                   items1 nil))
            ((funcall lt-than (first items1) (first items2))
             (setf (rest tmp) (cons (first items1) nil)
                   len (+ 1 len)
                   tmp (rest tmp)
                   items1 (rest items1)))
            ((funcall lt-than (first items2) (first items1))
             (setf (rest tmp) (cons (first items2) nil)
                   len (+ 1 len)
                   tmp (rest tmp)
                   items2 (rest items2)))
            (t
             ;; equal case
             (setf (rest tmp) (cons (first items1) nil)
                   len (+ 1 len)
                   tmp (rest tmp)
                   items1 (rest items1)
                   items2 (rest items2)))))
    (make-sorted-set :items (rest result)
                     :lt-pred lt-than
                     :length len)))

(defun sorted-set/intersection (set1 set2)
  "Intersect two sets, SET1 and SET2 whose lt-than predicates must be consistest.
Item equality is determined using LT-THAN:
X ~ Y == (and (not (lt-than X Y)) (not (lt-than Y X)))."
  (cl-assert (equal? (sorted-set/lt-pred set1)
                     (sorted-set/lt-pred set2))
             nil
             "Cannot merge sets with different lt predicates")
  (let* ((result (cons nil nil))
         (tmp result)
         (lt-than (sorted-set/lt-pred set1))
         (items1 (sorted-set/items set1))
         (items2 (sorted-set/items set2))
         (len 0))
    (while (and (not (null items1))
                (not (null items2)))
      (cond ((funcall lt-than (first items1) (first items2))
             (setf items1 (rest items1)))
            ((funcall lt-than (first items2) (first items1))
             (setf items2 (rest items2)))
            (t
             ;; equal case
             (setf (rest tmp) (cons (first items1) nil)
                   len (+ 1 len)
                   tmp (rest tmp)
                   items1 (rest items1)
                   items2 (rest items2)))))
    (make-sorted-set :items (rest result)
                     :lt-pred lt-than
                     :length len)))

(defun sorted-set/from-list (items lt-pred)
  "Construct sorted set from ITEMS list using LT-PRED predicate to sort
items and remove any duplicates."
  (let ((remove-duplicates
         (lambda (items)
           ;; items is sorted here
           (let* ((result (cons (first items) nil))
                  (tmp result)
                  (len 1))
             (setf items (rest items))
             (while items
               (when (funcall lt-pred (first tmp) (first items))
                 (setf (rest tmp) (cons (first items) nil)
                       tmp (rest tmp)
                       len (+ 1 len)))
               (setf items (rest items)))
             (values result len)))))
    (cl-multiple-value-bind (items len) (funcall remove-duplicates
                                              (sort (seq-copy items) lt-pred))
      (make-sorted-set :items items
                       :lt-pred lt-pred
                       :length len))))

(defun sorted-set/empty (lt-pred)
  "Construct empty set with given predicate, LT-PRED ."
  (make-sorted-set :items '()
                   :lt-pred lt-pred
                   :length 0))

(defun sorted-set/empty? (set)
  "Check whether SET is empty."
  (null (sorted-set/items set)))

(provide 'sorted-set)

;; Local Variables:
;; End:

;; sorted-set.el ends here
