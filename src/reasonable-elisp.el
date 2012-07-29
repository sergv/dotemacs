;; reasonable-elisp.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  3 July 2012
;; Description:

(defun filter (pred seq &rest args)
  (apply #'remove-if-not pred seq args))

(provide 'reasonable-elisp)

;; Local Variables:
;; lexical-binding: t
;; End:

;; reasonable-elisp.el ends here
