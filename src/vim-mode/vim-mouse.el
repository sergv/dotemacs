;; vim-mouse.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 19 March 2012
;; Description:

(require 'general-lisp-setup)


(defvar vim:current-mouse-event nil)

(defun vim:cmd-mouse-p (cmd)
  (get cmd 'mouse))

(defmacro* vim:defmousemotion (name (event-var &key (inclusive t))
                                    &rest body
                                    ;; (&rest args) &rest body
                                    )
  "Define mouse motion, which can only be inclusive or exclusive."
  (let ((type (if inclusive
                'inclusive
                'exclusive))
        (count nil)
        (argument nil)
        (params nil)
        (named-params nil)

        (doc nil)
        (event-receiver (gensym)))

    ;; extract documentation string
    (if (and (consp body)
             (cdr body)
             (stringp (car body)))
      (setq doc (car body)
            body (cdr body))
      (setq doc (format "VIM - motion (%s)" name)))

    `(progn
       (put ',name 'type ',type)
       (put ',name 'count nil)
       (put ',name 'argument nil)
       (put ',name 'mouse t)
       (put ',name 'function
            (function*
             (lambda (,event-var)
              ,@body)))
       (defun* ,name (,event-receiver)
         ,doc
         (interactive "e")
         (setf vim:current-mouse-event ,event-receiver)
         (if (vim:called-interactively-p)
           (vim:execute-command ',name)
           (apply (get ',name 'function) (list ,event-receiver)))))))

(defun vim--mouse-symbol/string/sexp (event exclusive)
  (let ((p (point)))
    (posn-set-point (event-end event))
    (cond
      ((lisp-at-beginning-of-sexp?)
       (let ((end (lisp-end-of-sexp-at-point)))
         (vim:make-motion :type 'inclusive ;;exclusive
                          :begin (point)
                          :end end
                          :has-begin nil)))
      ((lisp-at-end-of-sexp?)
       (let ((end (lisp-beginning-of-sexp-at-point)))
         (vim:make-motion :type 'inclusive ;;exclusive
                          :begin (point)
                          :end end
                          :has-begin nil)))
      ((lisp-at-beginning-of-string?)
       (let ((end (scan-sexps (point) 1)))
         (vim:make-motion :type exclusive
                          :begin (point)
                          :end end
                          :has-begin nil)))
      ((lisp-at-end-of-string?)
       (let ((end (scan-sexps (point) -1)))
         (vim:make-motion :type exclusive
                          :begin (point)
                          :end end
                          :has-begin nil)))
      (t
       (let ((bounds (bounds-of-thing-at-point 'symbol)))
         (if bounds
           (destructuring-bind (start . end)
               bounds
             (vim:make-motion :type exclusive
                              :begin start
                              :end end
                              :has-begin nil))
           (progn
             ;; return to start of this motion
             (goto-char p)
             (error "No symbol found around point"))))))))

(vim:defmousemotion vim:mouse-symbol/string/sexp (event)
  (vim--mouse-symbol/string/sexp event 'exclusive))



(provide 'vim-mouse)

;; Local Variables:
;; End:

;; vim-mouse.el ends here
