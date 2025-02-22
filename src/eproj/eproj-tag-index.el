;; eproj-tag-index.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 20 June 2018
;; Description:
;; This module provides a "tag index" data structure. This datastructure
;; serves to map symbol name to metadata that tracks where corresponding
;; symbol occurs.
;;
;; Conceptually, a tag index is a hash table from symbol names to non-empty
;; lists of ‘eproj-tag’ structures.

;;; eproj-tag

;; use this to debug type errors
;; (cl-defstruct (eproj-tag
;;                (:conc-name eproj-tag/))
;;   symbol ;; == name - string
;;   file   ;; string
;;   line   ;; number
;;   properties)

(eval-when-compile
  (require 'macro-util))

(require 'packing)

(defconst eproj-tag-complex-type-prop 'eproj--type)

(defun make-eproj-tag (file line type props)
  (declare (pure t) (side-effect-free t))
  (cl-assert (or (null props) (consp props)))
  (cl-assert (or (stringp type)
                 (null type)
                 (and (numberp type)
                      (<= 0 type)
                      (< type 256))))
  (cond
    ((stringp type)
     ;; Put complex type into props.
     (cons file
           (cons (packing-pack-pair line -1)
                 (cons (cons eproj-tag-complex-type-prop
                             type)
                       props))))
    (props
     (cons file
           (cons (packing-pack-pair line (or type -1))
                 props)))
    (t
     ;; Compact representation when there are no props.
     (cons file
           (packing-pack-pair line (or type -1))))))

(defun eproj-tag-p (tag-struct)
  (declare (pure t) (side-effect-free t))
  (and (consp tag-struct)
       (stringp (car tag-struct))
       (or (and (consp (cdr tag-struct))
                (integerp (cadr tag-struct)))
           (integerp (cdr tag-struct)))))

(defsubst eproj-tag/file (tag-struct)
  "Get the file that current tag came from. Always absolute."
  (declare (pure t) (side-effect-free t))
  (cl-assert (eproj-tag-p tag-struct))
  (car-sure tag-struct))

(defsubst eproj-tag/line (tag-struct)
  (declare (pure t) (side-effect-free t))
  (cl-assert (eproj-tag-p tag-struct))
  (let ((rest (cdr-sure tag-struct)))
    (packing-unpack-pair-car
     (if (consp rest)
         (car rest)
       rest))))

(defun eproj-tag/type (tag-struct)
  (declare (pure t) (side-effect-free t))
  (cl-assert (eproj-tag-p tag-struct))
  (let* ((rest (cdr-sure tag-struct))
         (res (packing-unpack-pair-cdr (if (consp rest)
                                           (car rest)
                                         rest))))
    (if (= -1 res)
        (eproj-tag/get-prop eproj-tag-complex-type-prop tag-struct)
      res)))

(defsubst eproj-tag/column (tag-struct)
  (declare (pure t) (side-effect-free t))
  (cl-assert (eproj-tag-p tag-struct))
  (eproj-tag/get-prop 'column tag-struct))

;; Return associative array of tag properties.
(defun eproj-tag/properties (tag-struct)
  (declare (pure t) (side-effect-free t))
  (cl-assert (eproj-tag-p tag-struct))
  (let ((rest (cdr-sure tag-struct)))
    (if (consp rest)
        (cdr rest)
      nil)))

(defsubst eproj-tag/get-prop (prop tag-struct)
  (declare (pure t) (side-effect-free t))
  (cl-assert (symbolp prop))
  (cl-assert (eproj-tag-p tag-struct))
  (cdr (assq prop (eproj-tag/properties tag-struct))))

(defsubst eproj-tag-index--create (tbl)
  (declare (pure t) (side-effect-free t))
  (cons 'eproj-tag-index tbl))

(defun empty-eproj-tag-index ()
  ;; Not pure because each invocation should get its own hash table.
  (declare (pure nil) (side-effect-free t))
  (eproj-tag-index--create (make-hash-table :test #'equal :size 997)))

(defun eproj-tag-index-p (index)
  (declare (pure t) (side-effect-free t))
  (and (consp index)
       (eq 'eproj-tag-index (car index))
       (hash-table-p (cdr index))))

(defsubst eproj-tag-index-size (index)
  (declare (pure t) (side-effect-free t))
  (cl-assert (eproj-tag-index-p index) nil "Invalid index: %s" index)
  (let ((n 0))
    (maphash (lambda (_ tags)
               (setf n (+ n (length tags))))
             (cdr-sure index))
    n))

(defun eproj-tag-index-add! (symbol file line type props index)
  (cl-assert (stringp symbol))
  (cl-assert (or (characterp type) (stringp type) (null type)))
  (cl-assert (file-name-absolute-p file))
  (cl-assert (eproj-tag-index-p index) nil "Invalid index: %s" index)
  (cl-assert (or (null props)
                 (and (listp props)
                      (--every (and (consp it)
                                    (symbolp (car it)))
                               props))))
  (let ((table (cdr-sure index)))
    (puthash symbol
             (cons (make-eproj-tag file line type props)
                   (gethash symbol table))
             table)))

(defsubst eproj-tag-index-get (key index &optional default)
  (declare (pure t))
  (cl-assert (stringp key))
  (cl-assert (eproj-tag-index-p index) nil "Invalid index: %s" index)
  (gethash key (cdr-sure index) default))

(defun eproj-tag-index-values-where-key-matches-regexp (re index &optional ignore-case)
  (cl-assert (eproj-tag-index-p index) nil "Invalid index: %s" index)
  (cl-assert (stringp re) nil "Invalid regexp: %s" re)
  (let ((case-fold-search ignore-case))
    (hash-table-entries-matching-re (cdr-sure index) re)))

(defsubst eproj-tag-index-keys (index)
  (declare (pure t))
  (cl-assert (eproj-tag-index-p index) nil "Invalid index: %s" index)
  (hash-table-keys (cdr-sure index)))

(defsubst eproj-tag-index-entries (index)
  (declare (pure t))
  (cl-assert (eproj-tag-index-p index) nil "Invalid index: %s" index)
  (hash-table->alist (cdr-sure index)))

(defun eproj-tag-index-drop-tags-from-file! (fname proj-root index)
  "Remove all tags that come from FNAME file. Tag file names will be expanded
relative to project root."
  (cl-assert (eproj-tag-index-p index))
  (let* ((old-tbl (cdr-sure index))
         (new-tbl (make-hash-table :test #'equal
                                   :size (hash-table-size old-tbl))))
    (maphash (lambda (key tags)
               (puthash key
                        (--filter (not
                                   (string= fname
                                            (expand-file-name (eproj-tag/file it)
                                                              proj-root)))
                                  tags)
                        new-tbl))
             old-tbl)
    (setcdr-sure index new-tbl)))

(defun eproj-tag-index-map-values! (f index)
  "Destructively rewrite values in INDEX index by mapping function F."
  (cl-assert (eproj-tag-index-p index))
  (let* ((old-tbl (cdr-sure index))
         (new-tbl (make-hash-table :test #'equal
                                   :size (hash-table-size old-tbl))))
    (maphash (lambda (key value)
               (puthash key (funcall f value) new-tbl))
             old-tbl)
    (setcdr-sure index new-tbl)))

(defun eproj-tag-index-merge! (index-a index-b)
  "Add all entries of INDEX-B to INDEX-A while combining values for
equal keys using `append'."
  (cl-assert (eproj-tag-index-p index-a))
  (cl-assert (eproj-tag-index-p index-b))
  (hash-table-merge-with!
   (lambda (_sym tags-a tags-b)
     (append tags-a tags-b))
   (cdr-sure index-a)
   (cdr-sure index-b)))

(defsubst eproj-tag-index-all-completions (s index)
  (cl-assert (eproj-tag-index-p index) nil "Invalid index: %s" index)
  (all-completions s (cdr-sure index)))

(provide 'eproj-tag-index)

;; Local Variables:
;; End:

;; eproj-tag-index.el ends here
