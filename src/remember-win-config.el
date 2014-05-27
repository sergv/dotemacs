;; remember-win-config.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 11 March 2012
;; Description:

(defparameter *remember-win-config-configurations* nil
  "Alist of (char win-config) pairs.")

(defun remember-win-config-store-configuration (char)
  (interactive "c")
  (setf *remember-win-config-configurations*
        (cons (list char (current-window-configuration))
              (remove* char
                       *remember-win-config-configurations*
                       :test #'char=
                       :key #'car))))

(defun remember-win-config-read-ascii-char ()
  (flet ((char-in-range? (c beg end)
                         (and (<= beg c)
                              (<= c end))))
    (let ((c (read-char-exclusive)))
      (if (or
           ;; [0-9]
           (char-in-range? c 48 57)
           ;; [A-Z]
           (char-in-range? c 65 90)
           ;; [a-z]
           (char-in-range? c 97 122))
        c
        (remember-win-config-read-ascii-char)))))

(defun remember-win-config-restore-configuration ()
  (interactive)
  (let* ((c (remember-win-config-read-ascii-char))
         (entry (find c *remember-win-config-configurations*
                      :test #'char=
                      :key #'car)))
    (if entry
      (set-window-configuration (cadr entry))
      (error "CONFIGURATION %s IS UNDEFINED" (char-to-string c)))))


(provide 'remember-win-config)

;; Local Variables:
;; End:

;; remember-win-config.el ends here
