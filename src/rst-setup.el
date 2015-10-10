;; rst-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 30 January 2012
;; Keywords:
;; Requirements:
;; Status:

(defun rst-setup ()
  (init-common :use-render-formula t
               :use-whitespace 'tabs-only
               :use-fci t))

(add-hook 'rst-mode-hook #'rst-setup)


(provide 'rst-setup)

;; Local Variables:
;; End:

;; rst-setup.el ends here
