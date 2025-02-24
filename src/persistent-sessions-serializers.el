;; persistent-sessions-serializers.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 25 August 2016
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'common)
(require 'persistent-sessions-error-reporting)
(require 'text-property-utils)

(defun sessions/store-value (lisp-value)
  "Encode LISP-VALUE into some representation that can be read reliably and which
can allows value to be decoded back fully.)"
  (cond
    ((stringp lisp-value)
     (sessions/store-string lisp-value))
    ((ring-p lisp-value)
     (sessions/store-ring lisp-value))
    ((sessions/proper-listp lisp-value)
     (sessions/store-proper-list lisp-value))
    ((vectorp lisp-value)
     (sessions/store-vector lisp-value))
    ((hash-table-p lisp-value)
     (sessions/store-hash-table lisp-value))
    (t
     (list 'literal-data
           lisp-value))))

(defun sessions/versioned/restore-value (version encoded-data)
  (sessions/assert-with-args (and (listp encoded-data)
                                  encoded-data
                                  (symbolp (car encoded-data)))
                             "sessions/restore-value: invalid format of encoded data: %s"
                             encoded-data)
  (pcase (car encoded-data)
    ('string
     (sessions/versioned/restore-string version encoded-data))
    ('ring
     (sessions/versioned/restore-ring version encoded-data))
    ('proper-list
     (sessions/versioned/restore-proper-list version encoded-data))
    ('vector
     (sessions/versioned/restore-vector version encoded-data))
    ('hash-table
     (sessions/versioned/restore-hash-table version encoded-data))
    ('literal-data
     (cadr encoded-data))
    (_
     (cond
       ((null version)
        encoded-data)
       ((>= version 2)
        (sessions/report-assert-failed
         (format "Invalid encoded data: %s" encoded-data)))
       (t
        (sessions/report-assert-failed
         (format "Invalid version: %s" version)))))))

;;;; Store/restore proper lists

(defun sessions/store-proper-list (lisp-list)
  (sessions/assert-with-args (sessions/proper-listp lisp-list)
                             "sessions/store-list: cannot store improper list: %s"
                             lisp-list)
  (list 'proper-list
        (-map #'sessions/store-value lisp-list)))

(defun sessions/versioned/restore-proper-list (version encoded-data)
  (sessions/assert-with-args (and (listp encoded-data)
                                  encoded-data
                                  (eq (car encoded-data) 'proper-list))
                             "Invalid tag of encoded proper list: %s"
                             (car encoded-data))
  (-map (lambda (x) (sessions/versioned/restore-value version x))
        (cadr encoded-data)))

;;;; Store/restore vectors

(defun sessions/store-vector (lisp-vector)
  (sessions/assert-with-args (vectorp lisp-vector)
                             "sessions/store-vector: cannot store non-vector: %s"
                             lisp-vector)
  (list 'vector
        (cl-map 'vector #'sessions/store-value lisp-vector)))

(defun sessions/versioned/restore-vector (version encoded-data)
  (sessions/assert (and (listp encoded-data)
                        encoded-data
                        (eq (car encoded-data) 'vector))
                   "Invalid tag of encoded vector")
  (cl-map 'vector
          (lambda (x) (sessions/versioned/restore-value version x))
          (cadr encoded-data)))

;;;; Store/restore hash tables

(defun sessions/store-hash-table (lisp-hash-table)
  (sessions/assert-with-args (hash-table-p lisp-hash-table)
                             "sessions/store-hash-table: cannot store non-hash-table: %s"
                             lisp-hash-table)
  (list 'hash-table
        (let ((kvs (hash-table->alist lisp-hash-table)))
          (dolist (kv kvs)
            (setf (car kv) (sessions/store-value (car kv))
                  (cdr kv) (sessions/store-value (cdr kv))))
          kvs)
        (hash-table-test lisp-hash-table)))

(defun sessions/versioned/restore-hash-table (version encoded-data)
  (sessions/assert (and (listp encoded-data)
                        encoded-data
                        (eq (car encoded-data) 'hash-table))
                   "Invalid tag of encoded hash table")
  (let ((kv-alist (cadr encoded-data))
        (test (caddr encoded-data)))
    (alist->hash-table
     (-map (lambda (kv)
             (cons (sessions/versioned/restore-value version (car kv))
                   (sessions/versioned/restore-value version (cdr kv))))
           kv-alist)
     test)))

;;;; Store/restore rings

(defun sessions/store-ring (ring)
  (sessions/assert-with-args (ring-p ring)
                             "sessions/store-ring: cannot store non-ring: %s"
                             ring)
  (list 'ring
        (sessions/map-ring #'sessions/store-value ring)))

(defun sessions/versioned/restore-ring (version encoded-data)
  (sessions/assert (eq 'ring (car encoded-data))
                   "Invalid tag of encoded ring")
  (let ((ring (cadr encoded-data)))
    (sessions/map-ring (lambda (x) (sessions/versioned/restore-value version x)) ring)))

;;;; Store/restore strings

(defun sessions/store-string (str &optional do-not-store-properties ignored-text-properties)
  (list 'string
        (split-string (base64-encode-string (string-as-unibyte (sessions/strip-text-properties str)))
                      "\n"
                      t)
        (if do-not-store-properties
            nil
          (sessions/get-all-text-properties-in-string
           str
           (append
            ;; These properties typicaly arise during copying and pasting and generally
            ;; contain unpleasant data...
            '(yank-handler
              vim--yank-handler
              ;; ... in particular this fellow contains structure with markers.
              magit-section)
            ignored-text-properties)))
        (multibyte-string-p str)))

(defun sessions/versioned/restore-string (_version encoded-data)
  (sessions/restore-string encoded-data))

(defun sessions/restore-string (encoded-data)
  (sessions/assert (eq 'string (car encoded-data))
                   "Invalid tag of encoded string")
  (let ((base64-lines (cadr encoded-data))
        (props (caddr encoded-data))
        (is-multibyte (cadddr encoded-data)))
    (sessions/assert-with-args base64-lines
                               "Error while restoring string from encoded data: %s"
                               encoded-data)
    (let ((str
           (with-temp-buffer
             (dolist (line base64-lines)
               (insert line))
             (funcall
              (if is-multibyte
                #'string-as-multibyte
                #'identity)
              (base64-decode-string
               (buffer-substring-no-properties (point-min) (point-max)))))))
      (sessions/apply-properties-to-string props str)
      str)))

(defun sessions/get-all-text-properties-in-string (string ignored-text-properties)
  "Get all suitable for serialisation text properties from STRING."
  (text-property-utils-get-all-text-properties-in-string string ignored-text-properties))

(defun sessions/apply-properties-to-string (props str)
  (text-property-utils-apply-properties-to-string! props str))

(defun sessions/map-ring (f ring)
  (sessions/assert (ring-p ring) "Not a ring")
  (let ((result (make-ring (ring-size ring))))
    (dotimes (n (ring-length ring))
      (ring-insert result
                   (funcall f
                            (ring-ref ring n))))
    result))

(defun sessions/proper-listp (x)
  "Check whether X is a proper list - i.e. sequence of cons cells ending in nil."
  (while (and x (consp x))
    (setf x (cdr x)))
  (null x))

(provide 'persistent-sessions-serializers)

;; Local Variables:
;; End:

;; persistent-sessions-serializers.el ends here
