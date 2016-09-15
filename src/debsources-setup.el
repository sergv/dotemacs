;; debsources-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 25 November 2011
;; Keywords:
;; Requirements:
;; Status:

;;;###autoload
(autoload 'debsources-mode "debsources" "Start debsources mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("sources\\.list\\'" . debsources-mode))

;;;###autoload
(defun debsources-setup ()
  (init-common :use-yasnippet nil))

;;;###autoload
(add-hook 'debsources-mode-hook #'debsources-setup)

(provide 'debsources-setup)

;; debsources-setup.el ends here
