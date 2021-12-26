;; packing.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 25 December 2021
;; Description:

(defun packing-pack-pair (a b)
  (declare (pure t) (side-effect-free t))
  (logior (ash a 32) b))

(defsubst packing-unpack-pair-car (x)
  (declare (pure t) (side-effect-free t))
  (ash x -32))

(defsubst packing-unpack-pair-cdr (x)
  (declare (pure t) (side-effect-free t))
  (logand #x00000000ffffffff x))

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
        (loop
          for i from 0 below butlast
          for j from 0 by 2
          do
          (setf (aref res i) (packing-pack-pair (aref xs j) (aref xs (+ j 1)))))
        (if (evenp orig-len)
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
        (loop
          for i from 0 below orig-len
          for j from 0 by 2
          do
          (let ((packed (aref xs i) ))
            (setf (aref res j)       (packing-unpack-pair-car packed)
                  (aref res (+ j 1)) (packing-unpack-pair-cdr packed))))
        (if (= 0 (aref res (- len 1)))
            (subseq res 0 (- len 1))
          res)))))

(defun packing-pack-list (xs)
  (declare (pure t) (side-effect-free t))
  (let* ((res (cons nil nil))
         (tmp res))
    (while xs
      (setf (cdr tmp) (cons (packing-pack-pair (car xs) (or (cadr xs) 0)) nil)
            tmp (cdr tmp)
            xs (cddr xs)))
    (cdr res)))

(defun packing-unpack-list (xs)
  (declare (pure t) (side-effect-free t))
  (let* ((res (cons nil nil))
         (prev nil)
         (tmp res))
    (while xs
      (let* ((packed (car xs))
             (last (cons (packing-unpack-pair-cdr packed)
                         nil))
             (butlast (cons (packing-unpack-pair-car packed)
                            last)))
        (setf (cdr tmp) butlast
              prev butlast
              tmp last
              xs (cdr xs))))
    (when (and (car tmp)
               (= 0 (car tmp)))
      (setf (cdr prev) nil))
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
  (clrhash packnig--pack-file-path--cache))

(defun packing--pack-file-path--component->idx (component)
  (aif (gethash component packing--file-path-component->idx-cache)
      it
    (let ((idx packing--file-path-free-idx))
      (incf packing--file-path-free-idx)
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

(defvar packnig--pack-file-path--cache (make-hash-table :test #'equal :size 997))

(defun packing-pack-file-path-cached (path)
  (cl-assert (stringp path))
  (if-let (cached (gethash path packnig--pack-file-path--cache))
      cached
    (let ((packed (packing-pack-file-path path)))
      (puthash path packed packnig--pack-file-path--cache)
      packed)))

(provide 'packing)

;; Local Variables:
;; End:

;; packing.el ends here
