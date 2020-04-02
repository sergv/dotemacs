;; idl-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 21 December 2013
;; Description:

(require 'common)
(require 'cc-setup)

;;;###autoload
(defun idl-setup ()
  (cc-setup :define-special-keys t)
  (setup-folding t nil))

;;;###autoload
(add-hook 'idl-mode-hook #'idl-setup)

(provide 'idl-setup)

;; idl-setup.el ends here
