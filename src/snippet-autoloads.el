;;; snippet-autoloads.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 28 August 2011
;; Keywords:
;; Requirements:
;; Status:

(autoload 'snippet-mode "yasnippet" nil t)
(add-to-list 'auto-mode-alist '("\\.snip\\'" . snippet-mode))

(autoload 'snippet-setup "snippet-setup")

(add-hook 'snippet-mode-hook #'snippet-setup)

(provide 'snippet-setup)

;;; snippet-autoloads.el ends here
