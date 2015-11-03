;; python-autoloads.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday,  4 November 2015
;; Description:

(require 'common)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python[0-9.]*" . python-mode))

(autoload 'python-setup "python-setup")
(autoload 'inferior-python-setup "python-setup")

(add-hook 'python-mode-hook #'python-setup)
(add-hook 'inferior-python-mode-hook #'inferior-python-setup)

(autoload 'switch-to-python "python-setup" nil t)

(provide 'python-autoloads)

;; Local Variables:
;; End:

;; python-autoloads.el ends here
