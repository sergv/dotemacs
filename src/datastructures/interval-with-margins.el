;; interval-with-margins.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 25 April 2025
;; Description:

(eval-when-compile
  (require 'cl))

(cl-defstruct interval-with-margins
  start ;; integer
  end   ;; integer
  margin-before ;; positive integer or nil
  margin-after  ;; positive integer or nil
  )

(defun mk-interval-with-margins (start end margin-before margin-after)
  (cl-assert (numberp start))
  (cl-assert (numberp end))
  (cl-assert (or (null margin-before) (and (numberp margin-before)
                                           (<= 0 margin-before))))
  (cl-assert (or (null margin-after) (and (numberp margin-after)
                                          (<= 0 margin-after))))
  (make-interval-with-margins
   :start start
   :end end
   :margin-before margin-before
   :margin-after margin-after))

(defun interval-with-margins-resolve-start (x)
  (aif (interval-with-margins-margin-before x)
      (progn
        (cl-assert (>= it 0))
        (- (interval-with-margins-start x) it))
    (interval-with-margins-start x)))

(defun interval-with-margins-resolve-end (x)
  (if (interval-with-margins-margin-before x)
      (interval-with-margins-end x)
    (aif (interval-with-margins-margin-after x)
        (progn
          (cl-assert (>= it 0))
          (+ (interval-with-margins-end x) it))
      (interval-with-margins-end x))))

(defun interval-with-margins-merge-intervals (x y)
  (let ((start-x (interval-with-margins-start x))
        (start-y (interval-with-margins-start y)))
    (if (<= start-x start-y)
        (let ((end-x (interval-with-margins-end x))
              (end-y (interval-with-margins-end y)))
          (if (< end-x end-y)
              (mk-interval-with-margins start-x
                                        end-y
                                        (interval-with-margins-margin-before x)
                                        (interval-with-margins-margin-after y))
            (mk-interval-with-margins start-x
                                      end-x
                                      (interval-with-margins-margin-before x)
                                      (interval-with-margins-margin-after x))))
      (interval-with-margins-merge-intervals y x))))

(defun interval-with-margins--overlap? (x y)
  (let ((start-y (interval-with-margins-start y)))
    (if (<= (interval-with-margins-start x) start-y)
        (let ((end-x (interval-with-margins-end x)))
          (or (>= end-x
                  start-y)
              (when-let* ((margin-after-x (interval-with-margins-margin-after x)))
                (>= (+ end-x margin-after-x)
                    start-y))
              (when-let* ((margin-before-y (interval-with-margins-margin-before y)))
                (>= end-x
                    (- start-y margin-before-y)))))
      (interval-with-margins--overlap? y x))))

(defun interval-with-margins-merge-overlapping! (intervals)
  (cl-assert (listp intervals))
  (setf internals (sort intervals
                        :lessp (lambda (x y) (< (interval-with-margins-start x) (interval-with-margins-start y)))
                        :in-place t))
  (let ((tmp intervals))
    (while tmp
      (if-let* ((rest (cdr tmp)))
          (let ((curr (car tmp))
                (next (car rest)))
            (if (interval-with-margins--overlap? curr next)
                (setf (cdr tmp) (cdr rest)
                      (car tmp) (interval-with-margins-merge-intervals curr next))
              (setf tmp rest)))
        (setf tmp rest)))
    intervals))

(provide 'interval-with-margins)

;; Local Variables:
;; End:

;; interval-with-margins.el ends here
