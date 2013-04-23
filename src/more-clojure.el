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

(defmacro more-clojure/comp-impl (functions
                                  fallback-function
                                  use-apply-for-last-func)
  "Optimization trick to expand chains of composed functions instead of
using loop as in `more-clojure/comp'. But if expansion could not be done (e.g.
some of FUNCTIONSS is an expression that is expected to be evaluated right
where comp is called) then FALLBACK-FUNCTION will be used."
  (block cannot-optimize
    (let* ((args-var (gensym))
           (strip-quotation
            (lambda (x)
              (if (and (list? x)
                       (not (null? x))
                       (memq (first x) '(quote function)))
                (first (rest x))
                x)))
           (make-call
            (lambda (expr last-arg use-apply)
              (let ((call-form (if use-apply 'apply 'funcall)))
                (pcase expr
                  (`(,(or `function `quote) ,func)
                   (if (and (not use-apply)
                            (symbol? func))
                     `(,func ,last-arg)
                     `(,call-form ,expr ,last-arg)))
                  (`(,(or `partial `apply-partially) ,func . ,args)
                   (let ((f (funcall strip-quotation func)))
                     (if (and (not use-apply)
                              (symbol? f))
                       `(,f ,@args ,last-arg)
                       `(,call-form ,func ,@args ,last-arg))))
                  (`(partial-first ,func . ,args)
                   (let ((f (funcall strip-quotation func)))
                     (if (and (not use-apply)
                              (symbol? f))
                       `(,f ,last-arg ,@args)
                       `(,call-form ,func ,last-arg ,@args))))
                  (some-expr
                   (cl-return-from cannot-optimize
                     `(,(funcall strip-quotation fallback-function)
                       ,f
                       ,g
                       ,@funcs))))))))
      (letrec ((iter
                (lambda (funcs)
                  (funcall make-call
                           (first funcs)
                           (if (not (null? (rest funcs)))
                             (funcall iter (rest funcs))
                             args-var)
                           (and (null? (rest funcs))
                                use-apply-for-last-func)))))
        `(lambda ,(if use-apply-for-last-func
               (list &rest ,args-var)
               (list args-var))
           ,(funcall iter functions))))))

(defun more-clojure/comp (f g &rest funcs)
  "Fallback function composition routine."
  (let ((functions (reverse (cons f (cons g funcs)))))
    (lambda (arg)
      (let ((result (funcall (first functions) arg)))
        (dolist (func (rest functions))
          (setf result (funcall func result)))
        result))))

(defmacro comp (f g &rest funcs)
  `(more-clojure/comp-impl ,(cons f (cons g funcs))
                           more-clojure/comp
                           nil))

(defun more-clojure/comp* (f g &rest funcs)
  "Fallback function composition routine."
  (let ((functions (reverse (cons f (cons g funcs)))))
    (lambda (arg)
      (let ((result (apply (first functions) arg)))
        (dolist (func (rest functions))
          (setf result (funcall func result)))
        result))))

(defmacro comp* (f g &rest funcs)
  `(more-clojure/comp-impl ,(cons f (cons g funcs))
                           more-clojure/comp*
                           t))


(defalias 'partial #'apply-partially)

(defun partial-first (f &rest args)
  "Just like `partial' but adds ARGS at the end of argument list when
F will be called."
  (lambda (&rest more-args)
    (apply f (append more-args args))))

(provide 'more-clojure)

;; Local Variables:
;; End:

;; more-clojure.el ends here
