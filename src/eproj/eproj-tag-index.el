;; eproj-tag-index.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 20 June 2018
;; Description:
;; This module provides a "tag index" data structure. This datastructure
;; serves to map

(defun empty-eproj-tag-index ()
  (cons 'eproj-tag-index (make-hash-table :test #'equal :size 997)))

(defun eproj-tag-index-p (index)
  (and (consp index)
       (eq 'eproj-tag-index (car index))
       (hash-table-p (cdr index))))

(defun eproj-tag-index-size (index)
  (hash-table-count (cdr index)))

(defun eproj-tag-index-add! (key value index)
  (cl-assert (stringp key))
  (puthash key value (cdr index)))

(defun eproj-tag-index-get (key index &optional default)
  (cl-assert (stringp key))
  (gethash key (cdr index) default))

(defun eproj-tag-index-values-where-key-matches-regexp (re index)
  (cl-assert (eproj-tag-index-p index) nil "Invalid index: %s" index)
  (cl-assert (stringp re) nil "Invalid regexp: %s" re)
  (hash-table-entries-matching-re (cdr index) re))

(defun eproj-tag-index-keys (index)
  (hash-table-keys (cdr index)))

(provide 'eproj-tag-index)

;; Local Variables:
;; End:

;; eproj-tag-index.el ends here
