;; interval-with-margins.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 25 April 2025
;; Description:

(eval-when-compile
  (require 'cl))

(cl-defstruct interval-with-margins
  start ;; Integer
  end   ;; Integer
  margin-before ;; Positive integer or nil
  margin-after  ;; Positive integer or nil

  ;; Positive integer or nil. Like margin but is only effective in the end and only when both finals are present.
  final-margin-before
  ;; Positive integer or nil. Like margin but is only effective in the end and only when both finals are present.
  final-margin-after)

(defun mk-interval-with-margins (start end margin-before margin-after final-margin-before final-margin-after)
  (cl-assert (numberp start))
  (cl-assert (numberp end))
  (cl-assert (or (null margin-before)
                 (and (numberp margin-before)
                      (<= 0 margin-before))))
  (cl-assert (or (null margin-after)
                 (and (numberp margin-after)
                      (<= 0 margin-after))))
  (cl-assert (or (null final-margin-before)
                 (and (numberp final-margin-before)
                      (<= 0 final-margin-before))))
  (cl-assert (or (null final-margin-after)
                 (and (numberp final-margin-after)
                      (<= 0 final-margin-after))))
  (make-interval-with-margins
   :start start
   :end end
   :margin-before margin-before
   :margin-after margin-after
   :final-margin-before final-margin-before
   :final-margin-after final-margin-after))

(defun interval-with-margins-resolved-start (x &optional consider-final?)
  (cl-assert (or (null (interval-with-margins-margin-before x))
                 (<= 0 (interval-with-margins-margin-before x))))
  (cl-assert (or (null (interval-with-margins-final-margin-before x))
                 (<= 0 (interval-with-margins-final-margin-before x))))
  (- (interval-with-margins-start x)
     (max (or (interval-with-margins-margin-before x) 0)
          (or (and consider-final?
                   ;; Both must be present to have an effect.
                   (interval-with-margins-final-margin-after x)
                   (interval-with-margins-final-margin-before x))
              0))))

(defun interval-with-margins-resolved-end (x &optional consider-final?)
  (cl-assert (or (null (interval-with-margins-margin-after x))
                 (<= 0 (interval-with-margins-margin-after x))))
  (cl-assert (or (null (interval-with-margins-final-margin-after x))
                 (<= 0 (interval-with-margins-final-margin-after x))))
  (if (interval-with-margins-margin-before x)
      (+ (interval-with-margins-end x)
         (or (and consider-final?
                  (interval-with-margins-final-margin-after x))
             0))
    (+ (interval-with-margins-end x)
       (max (or (interval-with-margins-margin-after x) 0)
            (or (and consider-final?
                     ;; Both must be present to have an effect.
                     (interval-with-margins-final-margin-before x)
                     (interval-with-margins-final-margin-after x))
                0)))))

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
                                        (interval-with-margins-margin-after y)
                                        (interval-with-margins-final-margin-before x)
                                        (interval-with-margins-final-margin-after y))
            x
            ;; todo: is this equivalent to ‘x’?
            ;; (mk-interval-with-margins start-x
            ;;                           end-x
            ;;                           (interval-with-margins-margin-before x)
            ;;                           (interval-with-margins-margin-after x))
            ))
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
