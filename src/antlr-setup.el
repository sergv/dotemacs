;; antlr-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 21 June 2013
;; Description:

;;;###autoload
(autoload 'antlr-mode "antlr-mode" nil t)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.g\\'" . antlr-mode))

;;;###autoload
(setf antlr-font-lock-maximum-decoration t)

;;;###autoload
(defun antlr-setup ()
  (init-common :use-yasnippet nil
               :use-render-formula nil
               :use-whitespace 'tabs-only)
  (setup-hs-minor-mode))

;;;###autoload
(add-hook 'antlr-mode-hook #'antlr-setup)

(provide 'antlr-setup)

;; Local Variables:
;; End:

;; antlr-setup.el ends here
