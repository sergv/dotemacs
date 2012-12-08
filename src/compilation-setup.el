;; compilation-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 29 February 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
 (require 'cl))

(require 'common)
(require 'compile)

(setf compilation-always-kill t)

(defconst *compilation-jump-error-regexp*
  "^\\(\\(?:\\(?:\\.\\.?\\)?/[^/\n\t]+\\)*?\\)/?[^/\n\t]+:\\([0-9]+\\):\\([0-9]+\\):"
  "Regexp which is used by `compilation-jump-to-next-error'
and `compilation-jump-to-prev-error' to detect errors
in compilation or related buffers")

(define-circular-jumps
    compilation-jump-to-next-error
  compilation-jump-to-prev-error
  *compilation-jump-error-regexp*
  (unless (compilation-buffer-p (current-buffer))
    (error "Not in a compilation buffer")))

;;; compilation info

(defvar *compile-caller-info* nil
  "Alist containing information about buffer, major mode etc.
from where current compile command was invoked. Should be cleared
up by functions in compilation-finish-functions.")

(defadvice compilation-start (before
                              compilation-start-store-info
                              activate
                              compile)
  "Record information about caller of compile command into
`*compile-caller-info*'"
  (setf *compile-caller-info* `((mode . ,major-mode)
                                (compile-command . ,compile-command)
                                (buffer . ,(current-buffer)))))



(provide 'compilation-setup)

;; Local Variables:
;; End:

;; compilation-setup.el ends here
