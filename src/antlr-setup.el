;; antlr-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 21 June 2013
;; Description:

(autoload 'antlr-mode "antlr-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.g\\'" . antlr-mode))

(setf antlr-font-lock-maximum-decoration t)

(defun antlr-setup ()
  (init-common :use-yasnippet nil :use-render-formula nil)
  (hs-minor-mode 1))

(add-hook 'antlr-mode-hook #'antlr-setup)

(provide 'antlr-setup)

;; Local Variables:
;; End:

;; antlr-setup.el ends here
