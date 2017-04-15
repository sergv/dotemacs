;; persistent-sessions-error-reporting.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 25 August 2016
;; Description:

(defun sessions/assert (condition msg)
  (unless condition
    (sessions/report-assert-failed msg)))

(defun sessions/assert-with-args (condition msg &rest args)
  (unless condition
    (let* ((truncate
            (lambda (arg)
              (let ((s (format "%S" arg)))
                (if (< (length s) 100)
                    s
                  (concat (substring s 0 (length s))
                          "...")))))
           (err-msg (apply #'format msg (-map truncate args))))
      (sessions/report-assert-failed err-msg))))

(put 'sessions/assert-failed 'error-conditions '(sessions/assert-failed error))

(defun sessions/report-assert-failed (msg)
  (signal 'sessions/assert-failed msg))

(defmacro sessions/report-and-ignore-asserts (&rest body)
  (declare (indent 0))
  `(condition-case err
       (progn ,@body)
     (sessions/assert-failed
      (message "Warning: assertion failed: %s" err))))

(provide 'persistent-sessions-error-reporting)

;; Local Variables:
;; End:

;; persistent-sessions-error-reporting.el ends here
