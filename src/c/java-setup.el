;; java-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 17 January 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'cc-setup)
(require 'eproj-setup)

(defun java-setup ()
  (cc-setup :define-special-keys nil)
  (setf c-basic-offset 4
        vim:shift-width 4)
  (setq-local c-indentation-indent-style "java")

  (setup-eproj-symbnav)
  (setup-outline-headers :header-start "/"
                         :header-symbol "*"
                         :length-min 3
                         :length-max 9))

(provide 'java-setup)

;; Local Variables:
;; End:

;; java-setup.el ends here
