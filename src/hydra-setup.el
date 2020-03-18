;; hydra-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 17 March 2020
;; Description:

(require 'hydra)
(require 'posframe)

(setf hydra-hint-display-type (if (posframe-workable-p)
                                  'posframe
                                'lv))

(defmacro defhydra-ext (name args &optional docstring &rest heads)
  "Like `defhydra' but also binds standard keys I expect to find across
all hydras in my setup."
  (declare (indent defun) (doc-string 3))
  `(defhydra ,name ,args
     ,docstring
     ,@heads
     ("<escape>" nil)))

(defmacro defhydra-derive (name parent args &optional docstring &rest heads)
  (declare (indent defun) (doc-string 4))
  `(defhydra-ext ,name ,args
     ,(when docstring (concat (hydra--prop parent "/docstring") "\n" (s-trim-left docstring)))
     ,@(cl-delete-duplicates
        (append (hydra--prop parent "/heads") heads)
        :key #'car
        :test #'equal)))

(provide 'hydra-setup)

;; Local Variables:
;; End:

;; hydra-setup.el ends here
