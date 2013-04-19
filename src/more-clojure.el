;; more-clojure.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 19 April 2013
;; Description:

(eval-when-compile (require 'cl-lib))


(defun first-safe (x)
  (car-safe x))

(defun rest-safe (x)
  (cdr-safe x))

(defun comp (f g &rest funcs)
  (let ((functions (reverse (cons f (cons g funcs)))))
    (lambda (&rest args)
      (let ((result (apply (first functions) args)))
        (dolist (func (rest functions))
          (setf result (funcall func result)))
        result))))

(defun partial (f &rest args)
  (lambda (&rest more-args)
    (apply f (append args more-args))))

(provide 'more-clojure)

;; Local Variables:
;; End:

;; more-clojure.el ends here
