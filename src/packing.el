;; packing.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 25 December 2021
;; Description:

;; most-positive-fixnum
;; =
;; #x 1f ff ff ff
;;    ff ff ff ff
;;
;; Divide it evenly into two, 1 bit at the start is lost
;; #x 1 | f ff ff ff |> f <| f ff ff ff
;; Now divide the middle f into pair of 2-bit slices, c and 3:
;; #x 1 | f ff ff ff |> c & 3 <| f ff ff ff
;;        4 8  8  8     2 | 2    4 8  8  8 = 30 = 1 + 29

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(if (eval-when-compile
      ;; If we’re not little-endian
      (not (equal (byteorder) ?l)))
    (progn
      (defalias 'packing32-pack-pair #'cons)
      (defalias 'packing32-unpack-pair-car #'car)
      (defalias 'packing32-unpack-pair-cdr #'cdr))
  (progn
    (defun packing32-pack-pair (a b)
      (declare (pure t) (side-effect-free t))
      (let ((a-neg? (< a 0))
            (b-neg? (< b 0)))
        ;; Store sign bit separately from number payload. I.e. the number is packed not
        ;; in 2's complement but as payload+sign bit.
        (logior (ash (logior (if a-neg?
                                 #x2000
                               #x0000)
                             (logand #x1fff (abs a)))
                     14)
                (logior (if b-neg?
                            #x2000
                          #x0000)
                        (logand #x1fff (abs b))))))

    (defun packing32-unpack-pair-car (x)
      (declare (pure t) (side-effect-free t))
      (let ((is-neg? (= #x8000000 (logand #x8000000 x))))
        (* (logand #x1fff (ash x -14))
           (if is-neg?
               -1
             1))))

    (defun packing32-unpack-pair-cdr (x)
      (declare (pure t) (side-effect-free t))
      (let ((is-neg? (= #x2000 (logand #x2000 x))))
        (* (logand #x1fff x)
           (if is-neg?
               -1
             1))))))

(if (eval-when-compile
      (or ;; If we’re not little-endian...
          (not (equal (byteorder) ?l))
          ;; ...or not at least 64 bit
          (<= (log most-positive-fixnum 2) 60)))

    (progn
      (defalias 'packing-pack-pair #'cons)
      (defalias 'packing-unpack-pair-car #'car)
      (defalias 'packing-unpack-pair-cdr #'cdr))

  (progn
    (defun packing-pack-pair (a b)
      (declare (pure t) (side-effect-free t))
      (let ((a-neg? (< a 0))
            (b-neg? (< b 0)))
        ;; Store sign bit separately from number payload. I.e. the number is packed not
        ;; in 2's complement but as payload+sign bit.
        (logior (ash (logior (if a-neg?
                                 #x20000000
                               #x00000000)
                             (logand #x1fffffff (abs a)))
                     30)
                (logior (if b-neg?
                            #x20000000
                          #x00000000)
                        (logand #x1fffffff (abs b))))))

    (defun packing-unpack-pair-car (x)
      (declare (pure t) (side-effect-free t))
      (let ((is-neg? (= #x0800000000000000 (logand #x0800000000000000 x))))
        (* (logand #x1fffffff (ash x -30))
           (if is-neg?
               -1
             1))))

    (defun packing-unpack-pair-cdr (x)
      (declare (pure t) (side-effect-free t))
      (let ((is-neg? (= #x20000000 (logand #x20000000 x))))
        (* (logand #x1fffffff x)
           (if is-neg?
               -1
             1))))))

;; 32 bit maximum on both 64 and 32 bit platforms.
;; =
;; 2^29
;; =
;; #x 1f ff ff ff
;;
;; Divide it evenly into two, 1 bit at the start is lost
;; #x 1 | f ff |> f <| f ff |
;; Now divide the middle f into pair of 2-bit slices, c and 3:
;; #x 1 | f ff |> c & 3 <| f ff
;;        4 8     2 | 2    4 8  = 14 = 1 + 13

(defun packing32-unpack-pair (x)
  (declare (pure t) (side-effect-free t))
  (cons (packing32-unpack-pair-car x) (packing32-unpack-pair-cdr x)))

(defun packing-unpack-pair (x)
  (declare (pure t) (side-effect-free t))
  (cons (packing-unpack-pair-car x) (packing-unpack-pair-cdr x)))

(defun packing--split-path (path)
  (declare (pure t) (side-effect-free t))
  (split-string path "/+"))

(defun packing--join-path (components)
  (declare (pure t) (side-effect-free t))
  (mapconcat #'identity components "/"))

(defun packing-pack-vec (xs)
  (declare (pure t) (side-effect-free t))
  (let ((orig-len (length xs)))
    (if (= 0 orig-len)
        []
      (let*
          (;; Add 1 to round up division result.
           (len (/ (+ orig-len 1) 2))
           (res (make-vector len 0))
           (butlast (- len 1)))
        (cl-loop
          for i from 0 below butlast
          for j from 0 by 2
          do
          (setf (aref res i) (packing-pack-pair (aref xs j) (aref xs (+ j 1)))))
        (if (cl-evenp orig-len)
            (setf (aref res butlast) (packing-pack-pair (aref xs (- orig-len 2)) (aref xs (- orig-len 1))))
          (setf (aref res butlast) (packing-pack-pair (aref xs (- orig-len 1)) 0)))
        res))))

(defun packing-unpack-vec (xs)
  (declare (pure t) (side-effect-free t))
  (let ((orig-len (length xs)))
    (if (= 0 orig-len)
        []
      (let* ((len (* 2 orig-len))
             (res (make-vector len 0)))
        (cl-loop
          for i from 0 below orig-len
          for j from 0 by 2
          do
          (let ((packed (aref xs i) ))
            (setf (aref res j)       (packing-unpack-pair-car packed)
                  (aref res (+ j 1)) (packing-unpack-pair-cdr packed))))
        (if (= 0 (aref res (- len 1)))
            (cl-subseq res 0 (- len 1))
          res)))))

(defun packing-pack-list (xs)
  (declare (pure t) (side-effect-free t))
  (let* ((res (cons nil nil))
         (tmp res))
    (while xs
      (setf tmp (setcdr-sure tmp
                             (cons (packing-pack-pair (car xs)
                                                      (or (cadr xs) 0))
                                   nil))
            xs (cddr xs)))
    (cdr res)))

(defun packing-unpack-list (xs)
  (declare (pure t) (side-effect-free t))
  (cl-assert (listp xs))
  (let* ((res (cons nil nil))
         (prev nil)
         (tmp res))
    (while xs
      (let* ((packed (car-sure xs))
             (last (cons (packing-unpack-pair-cdr packed)
                         nil))
             (butlast (cons (packing-unpack-pair-car packed)
                            last)))
        (setf prev butlast)
        (setcdr-sure tmp butlast)
        (setf tmp last
              xs (cdr-sure xs))))
    (let ((first (car-sure tmp)))
      (when (and first
                 (= 0 first))
        (setcdr-sure prev nil)))
    (cdr res)))

(defvar packing--file-path-free-idx 1)
(defvar packing--file-path-component->idx-cache (make-hash-table :test #'equal))
(defvar packing--file-path-idx->component-cache-capacity 32)
(defvar packing--file-path-idx->component-cache (make-vector packing--file-path-idx->component-cache-capacity nil))

(defun packing--reset-caches ()
  (setf packing--file-path-free-idx 1)
  (clrhash packing--file-path-component->idx-cache)
  (setf packing--file-path-idx->component-cache-capacity 32
        packing--file-path-idx->component-cache (make-vector packing--file-path-idx->component-cache-capacity 0))
  (clrhash packing--pack-file-path--cache))

(defun packing--pack-file-path--component->idx (component)
  (aif (gethash component packing--file-path-component->idx-cache)
      it
    (let ((idx packing--file-path-free-idx))
      (cl-incf packing--file-path-free-idx)
      (puthash component idx packing--file-path-component->idx-cache)

      (let ((j (- idx 1)))
        (when (= j packing--file-path-idx->component-cache-capacity)
          (setf packing--file-path-idx->component-cache-capacity (* 2 packing--file-path-idx->component-cache-capacity)
                packing--file-path-idx->component-cache (vconcat packing--file-path-idx->component-cache
                                                        (make-vector (length packing--file-path-idx->component-cache)
                                                                     nil))))
        (setf (aref packing--file-path-idx->component-cache j) component))

      idx)))

(defun packing--pack-file-path--idx->component (idx)
  (declare (pure t) (side-effect-free t))
  (aref packing--file-path-idx->component-cache (- idx 1)))

(defun packing-pack-file-path (path)
  (declare (pure t) (side-effect-free t))
  (packing-pack-list
   (-map #'packing--pack-file-path--component->idx (packing--split-path path))))

(defun packing-unpack-file-path (packed-path)
  (declare (pure t) (side-effect-free t))
  (packing--join-path
   (-map #'packing--pack-file-path--idx->component
         (packing-unpack-list packed-path))))

(defvar packing--pack-file-path--cache (make-hash-table :test #'equal :size 997))

(defun packing-pack-file-path-cached (path)
  (cl-assert (stringp path))
  (if-let (cached (gethash path packing--pack-file-path--cache))
      cached
    (let ((packed (packing-pack-file-path path)))
      (puthash path packed packing--pack-file-path--cache)
      packed)))

(provide 'packing)

;; Local Variables:
;; End:

;; packing.el ends here
