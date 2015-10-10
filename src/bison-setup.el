;; bison-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 23 June 2013
;; Description:

(autoload 'bison-mode "bison-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.y\\'" . bison-mode))

(defun bison-setup ()
  (init-common :use-yasnippet nil
               :use-render-formula nil
               :use-whitespace 'tabs-only)
  (hs-minor-mode 1))

(add-hook 'bison-mode-hook #'bison-setup)

(provide 'bison-setup)

;; Local Variables:
;; End:

;; bison-setup.el ends here
