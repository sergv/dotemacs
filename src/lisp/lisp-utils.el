;; lisp-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  9 January 2012
;; Keywords:
;; Requirements:
;; Status:


(defun looking-at-start-of-sexp? (&optional pos)
  (let ((char (char-after pos)))
    (when char
      (char= ?\( char))))

(defun looking-at-end-of-sexp? (&optional pos)
  (let ((char (char-after pos)))
    (when char
      (char= ?\) char))))


(provide 'lisp-utils)

;; Local Variables:
;; End:

;; lisp-utils.el ends here
