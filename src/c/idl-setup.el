;; idl-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 21 December 2013
;; Description:

(require 'common)
(require 'cc-setup)

(defun idl-setup ()
  (cc-setup :define-special-keys t
            :use-c-eldoc nil))

(provide 'idl-setup)

;; idl-setup.el ends here
