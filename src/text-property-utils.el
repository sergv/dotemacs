;; text-property-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 24 February 2025
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'common)

(cl-defstruct (text-property-ranges
               (:conc-name text-property-ranges/))
  ranges ;; List of either <number> or cons pairs (<number> . <number>).
         ;; Cons pair stands for (<start> . <end>) range, single number -
         ;; for singleton range (<start> . <start>).
         ;;
         ;; Later positions appear at the beginning of the list.
  )

(defun text-property-utils--empty-position-ranges ()
  (make-text-property-ranges :ranges nil))

(defun text-property-utils--position-ranges--add-range! (start end position-ranges)
  (cl-assert (and (numberp start)
                  (numberp end)
                  (< start end)))
  (let* ((ranges (text-property-ranges/ranges position-ranges))
         (merged-ranges
          (if ranges
              (let ((first-range (car ranges)))
                (cl-assert (consp first-range))
                (let ((range-end (cdr first-range)))
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
    (setf (text-property-ranges/ranges position-ranges)
          merged-ranges)
    position-ranges))

(defun text-property-utils-get-all-text-properties-in-string (string ignored-text-properties)
  (let* ((end (length string))
         (props (text-properties-at 0 string))
         (pos 0)
         (properties (make-hash-table :test #'eq)))
    (while (< pos end)
      (let ((change-pos (or (next-property-change pos string) end)))
        (let ((start pos)
              (end change-pos))
          (cl-loop
           for (key value) on props by #'cddr
           do
           (unless (memq key ignored-text-properties)
             (let ((value-table
                    (gethash key
                             properties
                             (make-hash-table :test #'equal))))
               (if-let* ((ranges (gethash value value-table)))
                   (text-property-utils--position-ranges--add-range! start end ranges)
                 (progn
                   (setf ranges (text-property-utils--empty-position-ranges))
                   (text-property-utils--position-ranges--add-range! start end ranges)
                   (puthash value ranges value-table)))
               (puthash key
                        value-table
                        properties)))))
        (setf props (text-properties-at change-pos string)
              pos change-pos)))
    (let ((results nil))
      (maphash (lambda (prop value-table)
                 (maphash (lambda (val ranges)
                            (push (list prop val (reverse (text-property-ranges/ranges ranges))) results))
                          value-table))
               properties)
      results)))

(defun text-property-utils-apply-properties-to-string! (props str)
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
;;                      (text-property-ranges/add-position
;;                       pos
;;                       (gethash value
;;                                value-table
;;                                (make-text-property-ranges)))
;;                      value-table)
;;             (puthash key
;;                      value-table
;;                      properties))
;;           (setf props (cddr props)))))
;;     (let ((results nil))
;;       (maphash (lambda (prop value-table)
;;                  (maphash (lambda (val ranges)
;;                             (push (list prop val (reverse (text-property-ranges/ranges ranges))) results))
;;                           value-table))
;;                properties)
;;       results)))

(provide 'text-property-utils)

;; Local Variables:
;; End:

;; text-property-utils.el ends here
