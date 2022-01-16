;; bimap.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 16 January 2022
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'macro-util))

(require 'sorted-set)

(cl-defstruct (bimap
               (:conc-name bimap/))
  forward-map   ;; hash table with #'equal comparison
  backward-map  ;; hash table with #'equal comparison
  size)

(defsubst bimap-size (bm)
  (hash-table-count (bimap/forward-map bm)))

(defun bimap-empty ()
  (make-bimap
   :forward-map (make-hash-table :test #'equal)
   :backward-map (make-hash-table :test #'equal)
   :size 0))

(defun bimap-lookup (key bm &optional default)
  (gethash key (bimap/forward-map bm) default))

(defun bimap-lookup-reverse (value bm &optional default)
  (gethash value (bimap/backward-map bm) default))

(defun bimap-insert (key value bm)
  (let* ((fwd         (bimap/forward-map bm))
         (bwd         (bimap/backward-map bm))
         (key-image   (gethash key fwd))
         (value-image (gethash value bwd)))
    (remhash key-image bwd)
    (remhash value-image fwd)
    (puthash key value fwd)
    (puthash value key bwd)))

(defun alist->bimap (items)
  (let ((bm (bimap-empty)))
    (dolist (entry items)
      (bimap-insert (car entry) (cdr entry) bm))
    bm))

(defun bimap-delete (key bm)
  (let ((fwd (bimap/forward-map bm))
        (bwd (bimap/backward-map bm)))
    (remhash (gethash key fwd) bwd)
    (remhash key fwd)
    bm))

(provide 'bimap)

;; Local Variables:
;; End:

;; bimap.el ends here
