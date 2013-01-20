;; java-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 17 January 2013
;; Description:

(require 'cc-setup)
(require 'ctags-setup)

(defun java-setup ()
  (cc-setup :define-special-keys nil)
  (setf c-basic-offset 4
        vim:shift-width 4))

(add-hook 'java-mode-hook #'java-setup)

;; Local Variables:
;; End:

;; java-setup.el ends here
