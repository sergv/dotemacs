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

(defun more-clojure/comp (f g &rest funcs)
  "Fallback function composition routine."
  (let ((functions (reverse (cons f (cons g funcs)))))
    (lambda (&rest args)
      (let ((result (apply (first functions) args)))
        (dolist (func (rest functions))
          (setf result (funcall func result)))
        result))))

(defmacro comp (f g &rest funcs)
  "Optimization trick to expand chains of composed functions instead of
using loop as in `more-clojure/comp'. But if expansion could not be done (e.g.
F, G, or some of FUNCS is an expression that is expected to be evaluated right
where comp is called) then `more-clojure/comp' will be used."
  (block cannot-optimize
    (let ((args-var (gensym))
          (make-call
           (lambda (func last-arg)
             (pcase func
               (`(,(or `function `quote) ,_)
                `(funcall ,func ,last-arg))
               (`(,(or `partial `apply-partially) . (,func . ,args))
                `(funcall ,func ,@args ,last-arg))
               (some-expr
                (cl-return-from cannot-optimize
                  `(more-clojure/comp ,f ,g ,@funcs)))))))
      (letrec ((iter
                (lambda (funcs)
                  (funcall make-call
                           (first funcs)
                           (if (not (null? (rest funcs)))
                             (funcall iter (rest funcs))
                             args-var)))))
        `(lambda (&rest ,args-var)
           ,(funcall iter (cons f (cons g funcs))))))))


(defalias 'partial #'apply-partially)

(provide 'more-clojure)

;; Local Variables:
;; End:

;; more-clojure.el ends here
