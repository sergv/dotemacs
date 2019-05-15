;; groovy-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 17 September 2017
;; Description:

;;;###autoload
(defun groovy-mode-setup ()
  (init-common :use-comment t :sp-slurp-sexp-insert-space nil)
  (setup-hs-minor-mode))

;;;###autoload
(add-hook 'groovy-mode-hook #'groovy-mode-setup)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

(provide 'groovy-mode-setup)

;; Local Variables:
;; End:

;; groovy-mode-setup.el ends here
