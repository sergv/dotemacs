;; dos-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  3 July 2014
;; Description:

(require 'common)

;;;###autoload
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bat\\'" . dos-mode))

;;;###autoload
(defun dos-setup ()
  (init-common :use-yasnippet nil
               :use-whitespace t))

;;;###autoload
(add-hook 'dos-mode-hook #'dos-setup)

(provide 'dos-setup)

;; Local Variables:
;; End:

;; dos-setup.el ends here
