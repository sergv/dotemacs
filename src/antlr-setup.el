;; antlr-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 21 June 2013
;; Description:

(eval-when-compile
  (defvar antlr-font-lock-maximum-decoration))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.g\\'" . antlr-mode))

;;;###autoload
(defun antlr-setup ()
  (init-common :use-yasnippet nil
               :use-render-formula nil
               :use-whitespace 'tabs-only)
  (setup-folding t nil))

;;;###autoload
(add-hook 'antlr-mode-hook #'antlr-setup)

(provide 'antlr-setup)

;; Local Variables:
;; End:

;; antlr-setup.el ends here
