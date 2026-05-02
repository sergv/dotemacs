;; interval-with-margins.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 25 April 2025
;; Description:

(eval-when-compile
  (require 'cl))

(cl-defstruct (interval-with-margins
               (:conc-name interval-with-margins/))
  ;; Positive integer, position
  start
  ;; Positive integer, position
  end

  ;; Non-negative integer or nil, offset.
  ;; Will be substracted from start unconditionally, intended to specify
  ;; spaces that come before start.
  spaces-before
  ;; Non-negative integer or nil, offset
  ;; Will be added to end unconditionally, intended to specify
  ;; spaces that come after end.
  spaces-after

  ;; Non-negative integer or nil, offset after spaces if they’re present.
  ;; Only one from ‘margin-before’ and ‘margin-after’ will have an effect.
  margin-before
  ;; Non-negative integer or nil, offse after spaces if they’re present.
  ;; Only one from ‘margin-before’ and ‘margin-after’ will have an effect.
  margin-after

  ;; Non-negative integer or nil. Like margin but is only effective in the end and only when both finals are present.
  final-margin-before
  ;; Non-negative integer or nil. Like margin but is only effective in the end and only when both finals are present.
  final-margin-after)

(defun mk-interval-with-margins (start end spaces-before spaces-after margin-before margin-after final-margin-before final-margin-after)
  (cl-assert (numberp start))
  (cl-assert (numberp end))
  (cl-assert (or (null margin-before)
                 (and (numberp margin-before)
                      (<= 0 margin-before))))
  (cl-assert (or (null margin-after)
                 (and (numberp margin-after)
                      (<= 0 margin-after))))
  (cl-assert (or (null spaces-before)
                 (and (numberp spaces-before)
                      (<= 0 spaces-before))))
  (cl-assert (or (null spaces-after)
                 (and (numberp spaces-after)
                      (<= 0 spaces-after))))
  (cl-assert (or (null final-margin-before)
                 (and (numberp final-margin-before)
                      (<= 0 final-margin-before))))
  (cl-assert (or (null final-margin-after)
                 (and (numberp final-margin-after)
                      (<= 0 final-margin-after))))
  (make-interval-with-margins
   :start start
   :end end
   :spaces-before spaces-before
   :spaces-after spaces-after
   :margin-before margin-before
   :margin-after margin-after
   :final-margin-before final-margin-before
   :final-margin-after final-margin-after))

(defun interval-with-margins-delete! (x &optional consider-final?)
  (let ((have-both-final-margins?
         (and consider-final?
              (interval-with-margins/final-margin-after x)
              (interval-with-margins/final-margin-before x))))
    (if-let* ((margin-before (interval-with-margins/margin-before x)))
        ;; Delete margin at the start.
        (delete-region
         (- (interval-with-margins/start x)
            (max (or (interval-with-margins/margin-before x)
                     0)
                 (or (and have-both-final-margins?
                          (interval-with-margins/final-margin-before x))
                     0)))
         (+ (interval-with-margins/end x)
            (or (and consider-final?
                     (interval-with-margins/final-margin-after x))
                ;; Delete all spaces after the end.
                (interval-with-margins/spaces-after x)
                0)))
      ;; Delete margin at the end.
      (delete-region
       (- (interval-with-margins/start x)
          (or (and have-both-final-margins?
                   (interval-with-margins/final-margin-before x))
              ;; Delete some spaces before the start.
              ;; Normalize multiple spaces by always keeping one space remaining.
              (max (- (or (interval-with-margins/spaces-before x)
                          0)
                      1)
                   0)
              0))
       (+ (interval-with-margins/end x)
          (max (or (interval-with-margins/margin-after x)
                   0)
               (or (and have-both-final-margins?
                        (interval-with-margins/final-margin-after x))
                   0)))))))

(defun interval-with-margins--force-merge-intervals (x y)
  "Merge two intervals into one regardless of whether they overlap."
  (let ((start-x (interval-with-margins/start x))
        (start-y (interval-with-margins/start y)))
    (if (<= start-x start-y)
        (let ((end-x (interval-with-margins/end x))
              (end-y (interval-with-margins/end y)))
          (if (< end-x end-y)
              (mk-interval-with-margins start-x
                                        end-y
                                        (interval-with-margins/spaces-before x)
                                        (interval-with-margins/spaces-after y)
                                        (interval-with-margins/margin-before x)
                                        (interval-with-margins/margin-after y)
                                        (interval-with-margins/final-margin-before x)
                                        (interval-with-margins/final-margin-after y))
            x
            ;; todo: is this equivalent to ‘x’?
            ;; (mk-interval-with-margins start-x
            ;;                           end-x
            ;;                           (interval-with-margins-margin-before x)
            ;;                           (interval-with-margins-margin-after x))
            ))
      (interval-with-margins--force-merge-intervals y x))))

(defun interval-with-margins--overlap? (x y)
  (let ((start-x (- (interval-with-margins/start x)
                    (or (interval-with-margins/spaces-before x)
                        0)))
        (start-y (- (interval-with-margins/start y)
                    (or (interval-with-margins/spaces-before y)
                        0))))
    (if (<= start-x start-y)
        (let ((end-x (+ (interval-with-margins/end x)
                        (or (interval-with-margins/spaces-after x)
                            0))))
          (or (>= end-x
                  start-y)
              (when-let* ((margin-after-x (interval-with-margins/margin-after x)))
                (>= (+ end-x margin-after-x)
                    start-y))
              (when-let* ((margin-before-y (interval-with-margins/margin-before y)))
                (>= end-x
                    (- start-y margin-before-y)))))
      (interval-with-margins--overlap? y x))))

(defun interval-with-margins-merge-overlapping! (intervals)
  (cl-assert (listp intervals))
  (setf internals (sort intervals
                        :lessp (lambda (x y) (< (interval-with-margins/start x) (interval-with-margins/start y)))
                        :in-place t))
  (let ((tmp intervals))
    (while tmp
      (if-let* ((rest (cdr tmp)))
          (let ((curr (car tmp))
                (next (car rest)))
            (if (interval-with-margins--overlap? curr next)
                (setf (cdr tmp) (cdr rest)
                      (car tmp) (interval-with-margins--force-merge-intervals curr next))
              (setf tmp rest)))
        (setf tmp rest)))
    intervals))

(provide 'interval-with-margins)

;; Local Variables:
;; End:

;; interval-with-margins.el ends here
