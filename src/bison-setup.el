;; bison-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 23 June 2013
;; Description:

;;;###autoload
(autoload 'bison-mode "bison-mode" nil t)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.y\\'" . bison-mode))

;;;###autoload
(defun bison-setup ()
  (init-common :use-yasnippet nil
               :use-render-formula nil
               :use-whitespace 'tabs-only)
  (hs-minor-mode 1))

;;;###autoload
(add-hook 'bison-mode-hook #'bison-setup)

(provide 'bison-setup)

;; Local Variables:
;; End:

;; bison-setup.el ends here
