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

;; (def-keys-for-map hydra-base-map
;;   )

(defmacro defhydra-ext (name body &optional docstring &rest heads)
  "Like `defhydra' but also binds standard keys I expect to find across
all hydras in my setup."
  (declare (indent defun) (doc-string 3))
  `(defhydra ,name ,body
     ,docstring
     ,@heads
     ("<escape>" nil)))

(provide 'hydra-setup)

;; Local Variables:
;; End:

;; hydra-setup.el ends here
