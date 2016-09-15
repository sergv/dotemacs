;; make-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday,  6 January 2016
;; Description:

;;;###autoload
(defun makefile-setup ()
  (init-common :use-yasnippet nil
               :use-whitespace nil
               :use-fci nil)
  (which-function-mode -1))

;;;###autoload
(add-hook 'makefile-mode-hook #'makefile-setup)

(provide 'make-setup)

;; Local Variables:
;; End:

;; make-setup.el ends here
