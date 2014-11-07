;; more-haskell.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 30 November 2013
;; Description:

(require 'common)

(defsubst concatMap (f xs)
  (mapcan (lambda (x) (copy-list (funcall f x))) xs))

(defun takeWhile (f xs)
  (let ((result nil))
    (while (and (not (null? xs))
                (funcall f (car xs)))
      (push (car xs) result))
    (nreverse result)))

(defsubst concat-lists (xss)
  (foldl #'append nil xss))

(provide 'more-haskell)

;; Local Variables:
;; End:

;; more-haskell.el ends here
