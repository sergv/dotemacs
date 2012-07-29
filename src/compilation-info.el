;;; compilation-info.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 22 December 2011
;; Keywords:
;; Requirements:
;; Status:


(require 'compile)

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


(provide 'compilation-info)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; compilation-info.el ends here
