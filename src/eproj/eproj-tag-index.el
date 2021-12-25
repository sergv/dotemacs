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


(defsubst make-eproj-tag (file line type props)
  (cl-assert (or (null props) (consp props)))
  (cons file (cons line (cons type props)))
  ;; (cons file (cons line props))
  )

(defsubst eproj-tag-p (tag-struct)
  (and (consp tag-struct)
       (stringp (car tag-struct))
       (consp (cdr tag-struct))
       (integerp (cadr tag-struct))))

;; (defsubst eproj-tag/symbol (tag-struct)
;;   (declare (pure t) (side-effect-free t))
;;   (car tag-struct))

(defsubst eproj-tag/file (tag-struct)
  "Get the file that current tag came from. Always absolute."
  (declare (pure t) (side-effect-free t))
  (car tag-struct))

(defsubst eproj-tag/line (tag-struct)
  (declare (pure t) (side-effect-free t))
  (cadr tag-struct))

(defsubst eproj-tag/type (tag-struct)
  (declare (pure t) (side-effect-free t))
  (caddr tag-struct))

(defsubst eproj-tag/column (tag-struct)
  (declare (pure t) (side-effect-free t))
  (eproj-tag/get-prop 'column tag-struct))

;; Return associative array of tag properties.
(defsubst eproj-tag/properties (tag-struct)
  (declare (pure t) (side-effect-free t))
  (cdddr tag-struct))

(defsubst eproj-tag/get-prop (prop tag-struct)
  (declare (pure t) (side-effect-free t))
  (cl-assert (symbolp prop))
  (cdr-safe (assq prop (cdddr tag-struct))))

(if (and nil use-foreign-libraries?)
    (progn
      (defalias 'empty-eproj-tag-index                           #'haskell-native--eproj-tag-index-empty)
      (defalias 'eproj-tag-index-p                               #'haskell-native--eproj-tag-index-p)
      (defalias 'eproj-tag-index-size                            #'haskell-native--eproj-tag-index-size)
      (defalias 'eproj-tag-index-add!                            #'haskell-native--eproj-tag-index-add!)
      (defalias 'eproj-tag-index-get                             #'haskell-native--eproj-tag-index-lookup)
      (defalias 'eproj-tag-index-values-where-key-matches-regexp #'haskell-native--eproj-tag-index-entries-matching-re)
      (defalias 'eproj-tag-index-keys                            #'haskell-native--eproj-tag-index-keys)
      (defalias 'eproj-tag-index-entries                         #'haskell-native--eproj-tag-index-entries)
      (defalias 'eproj-tag-index-drop-tags-from-file!            #'haskell-native--eproj-tag-index-drop-tags-from-file!)
      (defalias 'eproj-tag-index-merge!                          #'haskell-native--eproj-tag-index-map-union!)
      (defalias 'eproj-tag-all-completions                       #'haskell-native--eproj-tag-index-all-completions))
  (progn
    (defsubst eproj-tag-index--create (tbl)
      (cons 'eproj-tag-index tbl))

    (defun empty-eproj-tag-index ()
      (eproj-tag-index--create (make-hash-table :test #'equal :size 997)))

    (defun eproj-tag-index-p (index)
      (and (consp index)
           (eq 'eproj-tag-index (car index))
           (hash-table-p (cdr index))))

    (defun eproj-tag-index-size (index)
      (hash-table-count (cdr index)))

    (defun eproj-tag-index-add! (symbol file line type props index)
      (cl-assert (stringp symbol))
      (let ((table (cdr index)))
        (puthash symbol
                 (cons (make-eproj-tag file line type props)
                       (gethash symbol table))
                 table)))

    (defun eproj-tag-index-get (key index &optional default)
      (cl-assert (stringp key))
      (gethash key (cdr index) default))

    (defun eproj-tag-index-values-where-key-matches-regexp (re index &optional ignore-case)
      (cl-assert (eproj-tag-index-p index) nil "Invalid index: %s" index)
      (cl-assert (stringp re) nil "Invalid regexp: %s" re)
      (let ((case-fold-search ignore-case))
        (hash-table-entries-matching-re (cdr index) re)))

    (defun eproj-tag-index-keys (index)
      (hash-table-keys (cdr index)))

    (defun eproj-tag-index-entries (index)
      (hash-table->alist (cdr index)))

    (defun eproj-tag-index-drop-tags-from-file! (fname proj-root index)
      "Remove all tags that come from FNAME file. Tag file names will be expanded
relative to project root."
      (cl-assert (eproj-tag-index-p index))
      (let* ((old-tbl (cdr index))
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
        (setcdr index new-tbl)))

    (defun eproj-tag-index-map-values! (f index)
      "Destructively rewrite values in INDEX index by mapping function F."
      (cl-assert (eproj-tag-index-p index))
      (let* ((old-tbl (cdr index))
             (new-tbl (make-hash-table :test #'equal
                                       :size (hash-table-size old-tbl))))
        (maphash (lambda (key value)
                   (puthash key (funcall f value) new-tbl))
                 old-tbl)
        (setcdr index new-tbl)))

    (defun eproj-tag-index-merge! (index-a index-b)
      "Add all entries of INDEX-B to INDEX-A while combining values for
equal keys using `append'."
      (cl-assert (eproj-tag-index-p index-a))
      (cl-assert (eproj-tag-index-p index-b))
      (hash-table-merge-with!
       (lambda (_sym tags-a tags-b)
         (append tags-a tags-b))
       (cdr index-a)
       (cdr index-b)))

    (defun eproj-tag-index-all-completions (s index)
      (all-completions s (cdr index)))))

(provide 'eproj-tag-index)

;; Local Variables:
;; End:

;; eproj-tag-index.el ends here
