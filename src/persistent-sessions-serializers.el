;; persistent-sessions-serializers.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 25 August 2016
;; Description:

(require 'common)
(require 'persistent-sessions-error-reporting)

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

(defun sessions/store-string (str &optional do-not-store-properties)
  (list 'string
        (split-string (base64-encode-string (sessions/strip-text-properties str))
                      "\n"
                      t)
        (if do-not-store-properties
          nil
          (sessions/get-all-text-properties-in-string str))))

(defun sessions/versioned/restore-string (version encoded-data)
  (sessions/restore-string encoded-data))

(defun sessions/restore-string (encoded-data)
  (sessions/assert (eq 'string (car encoded-data))
                   "Invalid tag of encoded string")
  (let ((base64-lines (cadr encoded-data))
        (props (caddr encoded-data)))
    (sessions/assert-with-args base64-lines
                               "Error while restoring string from encoded data: %s"
                               encoded-data)
    (let ((str
           (with-temp-buffer
             (dolist (line base64-lines)
               (insert line))
             (base64-decode-string
              (buffer-substring-no-properties (point-min) (point-max))))))
      (sessions/apply-properties-to-string props str)
      str)))


(cl-defstruct (sessions/position-ranges
               (:conc-name sessions/position-ranges/))
  ranges ;; List of either <number> or cons pairs (<number> . <number>).
         ;; Cons pair stands for (<start> . <end>) range, single number -
         ;; for singleton range (<start> . <start>).
         ;;
         ;; Later positions appear at the beginning of the list.
  )

(defun sessions/empty-position-ranges ()
  (make-sessions/position-ranges :ranges nil))

;; (defun sessions/position-ranges/add-position (pos position-ranges)
;;   (cl-assert (numberp pos))
;;   (let* ((ranges (sessions/position-ranges/ranges position-ranges))
;;          (new-ranges
;;           (if ranges
;;             (let ((range (car ranges)))
;;               (cond
;;                 ((numberp range)
;;                  (cl-assert (< range pos)
;;                             nil
;;                             "Must add positions in increasing order only")
;;                  (if (= (+ range 1) pos)
;;                    (progn
;;                      (setf (car ranges) (cons range pos))
;;                      ranges)
;;                    (cons pos ranges)))
;;                 ((consp range)
;;                  (let ((start (car range))
;;                        (end (cdr range)))
;;                    (cl-assert (< end pos)
;;                               nil
;;                               "Must add positions in increasing order only")
;;                    (if (= (+ end 1) pos)
;;                      (progn
;;                        (setf (cdr range) pos)
;;                        ranges)
;;                      (cons pos ranges))))
;;                 (t
;;                  (error "Invalid range: %s" range))))
;;             (list pos))))
;;     (setf (sessions/position-ranges/ranges position-ranges)
;;           new-ranges)
;;     position-ranges))

(defun sessions/position-ranges/add-range (start end position-ranges)
  (cl-assert (and (numberp start)
                  (numberp end)
                  (< start end)))
  (let* ((ranges (sessions/position-ranges/ranges position-ranges))
         (new-ranges
          (if ranges
            (let ((first-range (car ranges)))
              (cl-assert (consp first-range))
              (let ((range-start (car first-range))
                    (range-end (cdr first-range)))
                (cl-assert (numberp range-start))
                (cl-assert (numberp range-end))
                (cl-assert (<= range-end start)
                           nil
                           "Must add positions in increasing order only. range-end = %s, start = %s"
                           range-end
                           start)
                (if (= range-end start)
                  (progn
                    (setf (cdr first-range) end)
                    ranges)
                  (cons (cons start end) ranges))))
            (list (cons start end)))))
    (setf (sessions/position-ranges/ranges position-ranges)
          new-ranges)
    position-ranges))

(defun sessions/get-all-text-properties-in-string (string)
  "Get all text properties from STRING."
  (let* ((end (length string))
         (props (text-properties-at 0 string))
         (pos 0)
         (properties (make-hash-table :test #'eq)))
    (while (< pos end)
      (let ((change-pos (or (next-property-change pos string) end)))
        (let ((start pos)
              (end change-pos))
          (loop
            for (key value) on props by #'cddr
            do
            (let ((value-table
                   (gethash key
                            properties
                            (make-hash-table :test #'equal))))
              (puthash value
                       (sessions/position-ranges/add-range
                        start
                        end
                        (gethash value
                                 value-table
                                 (sessions/empty-position-ranges)))
                       value-table)
              (puthash key
                       value-table
                       properties))))
        (setf props (text-properties-at change-pos string)
              pos change-pos)))
    (let ((results nil))
      (maphash (lambda (prop value-table)
                 (maphash (lambda (val ranges)
                            (push (list prop val (reverse (sessions/position-ranges/ranges ranges))) results))
                          value-table))
               properties)
      results)))

;; (defun sessions/get-all-text-properties-in-string (str)
;;   (let ((properties
;;          (make-hash-table :test #'eq)))
;;     (dotimes (pos (length str))
;;       (let ((props (text-properties-at pos str)))
;;         (while props
;;           (let* ((key (car props))
;;                  (value (cadr props))
;;                  (value-table
;;                   (gethash key
;;                            properties
;;                            (make-hash-table :test #'equal))))
;;             (puthash value
;;                      (sessions/position-ranges/add-position
;;                       pos
;;                       (gethash value
;;                                value-table
;;                                (make-sessions/position-ranges)))
;;                      value-table)
;;             (puthash key
;;                      value-table
;;                      properties))
;;           (setf props (cddr props)))))
;;     (let ((results nil))
;;       (maphash (lambda (prop value-table)
;;                  (maphash (lambda (val ranges)
;;                             (push (list prop val (reverse (sessions/position-ranges/ranges ranges))) results))
;;                           value-table))
;;                properties)
;;       results)))

(defun sessions/apply-properties-to-string (props str)
  (dolist (entry props)
    (sessions/report-and-ignore-asserts
      (sessions/assert-with-args (and (listp entry)
                                      (= 3 (length entry)))
                                 "Invalid property entry: %s"
                                 entry)
      (let ((prop-name (car entry))
            (prop-value (cadr entry))
            (prop-positions (caddr entry)))
        (sessions/assert-with-args (symbolp prop-name)
                                   "Invalid property name: %s"
                                   prop-name)
        (dolist (pos prop-positions)
          (sessions/assert-with-args (or (numberp pos)
                                         (consp pos))
                                     "Invalid property range: %s"
                                     pos)
          (let ((start (car pos))
                (end (cdr pos)))
            (put-text-property start end prop-name prop-value str)))))))

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
