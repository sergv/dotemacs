;; s-extras.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  6 February 2022
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'common-small)

(defun s-extras-count-chars-in-string (c str)
    "Function to count number of times character C occurs in string STR.

Implementation is very straightforward and because of that fast and reliable."
    (cl-assert (characterp c))
    (cl-assert (stringp str))
    (let ((res (comp-hint-fixnum 0))
          (i (comp-hint-fixnum 0))
          (len (length str)))
      (while (< i len)
        (when (eq c (aref str i))
          (setq res (1+ (comp-hint-fixnum res))))
        (cl-incf i))
      res))

(defun s-extras-replace-char! (old new s)
  "Replace all OLD characters with NEW in the S string by mutation."
  (cl-assert (stringp s))
  (cl-assert (characterp old))
  (cl-assert (characterp new))
  (let ((end (comp-hint-fixnum (length s)))
        (i (comp-hint-fixnum 0)))
    (while (< (comp-hint-fixnum i) (comp-hint-fixnum end))
      (when (eq (aref s (comp-hint-fixnum i)) old)
        (setf (aref s (comp-hint-fixnum i)) new))
      (cl-incf i))
    s))

(provide 's-extras)

;; Local Variables:
;; End:

;; s-extras.el ends here
