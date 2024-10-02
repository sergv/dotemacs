;; treesit-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 June 2024
;; Description:

(defsubst treesit-haskell--current-node ()
  (treesit-haskell--node-at (point)))

(defun treesit-haskell--node-at (pos)
  (when (derived-mode-p 'haskell-ts-mode)
    (treesit-node-at pos
                     ;; Hoping the parser will get reused, should be safe for
                     ;; haskell-ts-mode and its derivatives.
                     (treesit-parser-create 'haskell))))

(defmacro* treesit-with-evaluated-anchor-and-offset
    ((evaluated-anchor-pos-var anchor)
     (evaluated-offset-num-var offset)
     &rest body)
  (declare (indent 2))
  (let ((anchor-var '#:anchor)
        (offset-var '#:offset))
    `(let ((,anchor-var ,anchor)
           (,offset-var ,offset))
       (let ((anchor-pos (cond
                           ((treesit-node-p ,anchor-var)
                            (treesit-node-start ,anchor-var))
                           ((number-or-marker-p ,anchor-var)
                            ,anchor-var)
                           ((null ,anchor-var)
                            nil)
                           (t
                            (error "Unexpected anchor: ‘%s’" ,anchor-var))))
             (offset-num (cond
                           ((functionp ,offset-var)
                            (funcall ,offset-var ,anchor-var))
                           ((numberp ,offset-var)
                            ,offset-var)
                           (t
                            (error "Unexpected offset: ‘%s’" ,offset-var)))))
         ,@body))))

(provide 'treesit-utils)

;; Local Variables:
;; End:

;; treesit-utils.el ends here
