;; nested-hash-tables.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 27 January 2026
;; Description:

(eval-when-compile
  (require 'cl))

(require 'dash)

;; Nested hash tables that allow to aggregate data differently.
(cl-defstruct (nested-hash-tables
               (:constructor make--nested-hash-tables)
               (:conc-name nested-hash-tables/))
  data        ;; chain of hash-tables
  field-specs ;; list of (<lamda to get key value> <comparison-pred>) entries
  )

(defun mk-nested-hash-tables (field-specs)
  (cl-assert (and field-specs
                  (not (null field-specs))
                  (listp field-specs)
                  (--all? (and (consp it)
                               (functionp (car it))
                               (functionp (cdr it)))
                          field-specs)))
  (make--nested-hash-tables
   :data (make-hash-table :test (cdar field-specs))
   :field-specs field-specs))

(defun nested-hash-tables/gethash (key hash-tables &optional def)
  (let ((table (nested-hash-tables/data hash-tables))
        (result def))
    (cl-loop
     for spec-entry on (nested-hash-tables/field-specs hash-tables)
     while table
     do
     (let* ((spec (car spec-entry))
            (get-key (car spec))
            (next-spec (cdr spec-entry))
            (current-level-key (funcall get-key key)))
       (if next-spec
           (setf table
                 (gethash current-level-key table))
         (setf result
               (gethash current-level-key table def)))))
    result))

(defun nested-hash-tables/add-kv! (key value hash-tables)
  (let ((table (nested-hash-tables/data hash-tables)))
    (cl-loop
     for spec-entry on (nested-hash-tables/field-specs hash-tables)
     do
     (let* ((spec (car spec-entry))
            (get-key (car spec))
            (next-spec (cdr spec-entry))
            (current-level-key (funcall get-key key))
            (next-value
             (if next-spec
                 (or (gethash current-level-key table)
                     (make-hash-table :test (cdr spec)))
               value)))
       (puthash current-level-key
                next-value
                table)
       (setf table next-value))))
  hash-tables)

(defun nested-hash-tables/add! (key hash-tables)
  (nested-hash-tables/add-kv! key key hash-tables))

(defun nested-hash-tables/maphash (f hash-tables)
  (let ((user-value-depth
         (length (nested-hash-tables/field-specs hash-tables))))
    (letrec ((handle-data
              (lambda (depth)
                (lambda (key value)
                  (if (= depth user-value-depth)
                      (funcall f key value)
                    (maphash (funcall handle-data (+ depth 1))
                             value))))))
      (maphash (funcall handle-data 1)
               (nested-hash-tables/data hash-tables)))))

(defun alist->nested-hash-tables (field-specs xs)
  (let ((tables (mk-nested-hash-tables field-specs)))
    (dolist (x xs)
      (nested-hash-tables/add-kv! (car x) (cdr x) tables))
    tables))

(defun nested-hash-tables->alist (hash-tables)
  (let ((result nil))
    (nested-hash-tables/maphash (lambda (k v)
                                  (push (cons k v) result))
                                hash-tables)
    result))

(provide 'nested-hash-tables)

;; Local Variables:
;; End:

;; nested-hash-tables.el ends here
