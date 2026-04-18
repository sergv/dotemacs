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

(defun s-extras--strip-terminal-save-restore-cursor-escape-sequences! (str)
  (let* ((i 0)
         (j 0)
         (len (length str))
         (dest str))
    (while (< i len)
      (let ((c (aref str i)))
        (if (eq c 27)
            (let ((k (+ i 1)))
              (when (< k len)
                (let ((c2 (aref str k)))
                  (if (or (eq c2 ?7)
                          (eq c2 ?8))
                      (progn
                        (cl-incf i 2))
                    (progn
                      (setf (aref dest j) c)
                      (cl-incf i)
                      (cl-incf j))))))
          (progn
            (setf (aref dest j) c)
            (cl-incf i)
            (cl-incf j)))))
    (substring dest nil j)))

(defun s-extras--strip-terminal-save-restore-cursor-escape-sequences (str)
  (let* ((i 0)
         (j 0)
         (len (length str))
         (dest (make-string len 0)))
    (while (< i len)
      (let ((c (aref str i)))
        (if (eq c 27)
            (let ((k (+ i 1)))
              (when (< k len)
                (let ((c2 (aref str k)))
                  (if (or (eq c2 ?7)
                          (eq c2 ?8))
                      (progn
                        (cl-incf i 2))
                    (progn
                      (setf (aref dest j) c)
                      (cl-incf i)
                      (cl-incf j))))))
          (progn
            (setf (aref dest j) c)
            (cl-incf i)
            (cl-incf j)))))
    (substring dest nil j)))

(provide 's-extras)

;; Local Variables:
;; End:

;; s-extras.el ends here
