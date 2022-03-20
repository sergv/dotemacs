;; bisect.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 16 January 2022
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(defun bisect (item items start end eq? less?)
  "Binary search. Returns index into vector ITEMS.
LESS? and EQ? are predicates taking item and an element of ITEMS.

START is inclusive and END is exclusive in ITEMS."
  (declare (pure t) (side-effect-free t))
  ;; if you doubt the implementation and want to improve it make sure
  ;; tests do pass
  (cl-assert (< start end))
  (while (< start end)
    (let* ((mid (/ (+ end start) 2))
           (mid-item (aref items mid)))
      (cond ((funcall less? item mid-item)
             (setf end mid))
            ((funcall eq? item mid-item)
             (setf start mid
                   end mid))
            (t
             (setf start (+ mid 1))))))
  start)

(defun bisect-find (items start end eq? less?)
  "Binary search. Returns index into vector ITEMS.
LESS? and EQ? are predicates on elements of ITEMS.

EQ? should return non-nil on an item we want the search to stop on.
LESS? should return non-nil if item we’re looking for is *smaller* than the
provided item.

START is inclusive and END is exclusive in ITEMS."
  (declare (pure t) (side-effect-free t))
  ;; if you doubt the implementation and want to improve it make sure
  ;; tests do pass
  (cl-assert (< start end))
  (while (< start end)
    (let* ((mid (/ (+ end start) 2))
           (mid-item (aref items mid)))
      (cond ((funcall less? mid-item)
             (setf end mid))
            ((funcall eq? mid-item)
             (setf start mid
                   end mid))
            (t
             (setf start (+ mid 1))))))
  start)

(defun bisect-fixnum (item items start end)
  "‘bisect’ specialized to integers"
  (declare (pure t) (side-effect-free t))
  ;; if you doubt the implementation and want to improve it make sure
  ;; tests do pass
  (cl-assert (< start end))
  (while (< start end)
    (let* ((mid (/ (+ end start) 2))
           (mid-item (aref items mid)))
      (cl-assert (fixnump mid-item))
      (cond ((< item mid-item)
             (setf end mid))
            ((eq item mid-item)
             (setf start mid
                   end mid))
            (t
             (setf start (+ mid 1))))))
  start)

(defun bisect-leftmost (item items start end eq? less?)
  "Similar to `bisect' but returns smallest index, idx, in ITEMS for which
\(funcall eq? item (aref items idx)) is true."
  (declare (pure t) (side-effect-free t))
  (let ((idx (bisect item items start end eq? less?)))
    (while (and (> idx 0)
                (funcall eq? item (aref items (- idx 1))))
      (setf idx (- idx 1)))
    idx))

(defun bisect-rightmost (item items start end eq? less?)
  "Similar to `bisect' but returns largest index, idx, in ITEMS for which
\(funcall eq? item (aref items idx)) is true."
  (declare (pure t) (side-effect-free t))
  (let ((idx (bisect item items start end eq? less?))
        (max-idx (- (length items) 1)))
    (while (and (< idx max-idx)
                (funcall eq? item (aref items (+ idx 1))))
      (setf idx (+ idx 1)))
    idx))

(provide 'bisect)

;; Local Variables:
;; End:

;; bisect.el ends here
