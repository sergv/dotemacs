;; nanothunk.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 18 March 2026
;; Description:
;;
;; Extra lightweight thunks that add as little overhead as I managed.

(defmacro nanothunk-delay (&rest body)
  "Delay the evaluation of BODY."
  (declare (debug (def-body)))
  (cl-assert lexical-binding)
  `(let ((forced nil)
         (val (lambda () ,@body)))
     (cons forced val)))

(defsubst nanothunk-make-evaluated (x)
  "Create thunk that’s already been evaluated."
  (cons t x))

(defsubst nanothunk-p (x)
  (consp x))

(defsubst nanothunk-force (thunk)
  "Force the evaluation of DELAYED.
The result is cached and will be returned on subsequent calls
with the same DELAYED argument."
  (if (car thunk)
      (cdr thunk)
    (setf (car thunk) t
          (cdr thunk) (funcall (cdr thunk)))))

(defsubst nanothunk-evaluated-p (thunk)
  "Check whether THUNK is evaluated."
  (car thunk))

(provide 'nanothunk)

;; Local Variables:
;; End:

;; nanothunk.el ends here
