;; cmdline.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 12 April 2026
;; Description:

(eval-when-compile
  (require 'cl))

(require 's)

;; Structure to represent all arguments needed to call a process
;; and show commands to a user in a presentable way.
(cl-defstruct (cmdline
               (:constructor make--cmdline))
  (exe      nil :read-only t) ;; string
  (args     nil :read-only t) ;; list of strings
  (rendered nil :read-only t) ;; list of strings with properties
  )

;;;###autoload
(cl-defun make-cmdline (&key exe args rendered)
  (cl-assert (stringp exe))
  (cl-assert (listp args))
  (cl-assert (-all? #'stringp args))
  (cl-assert (listp rendered))
  (cl-assert (-all? #'stringp rendered))
  (make--cmdline
   :exe exe
   :args args
   :rendered (or rendered
                 (cons exe args))))

;;;###autoload
(defun cmdline-to-executable-command (x)
  "Return list of arguments suitable to pass to ‘make-process’."
  (cons (cmdline-exe x) (cmdline-args x)))

;;;###autoload
(defun cmdline-to-pretty-command (x)
  "Return rendered string to show to user."
  (s-join " " (cmdline-rendered x)))

(provide 'cmdline)

;; Local Variables:
;; End:

;; cmdline.el ends here
