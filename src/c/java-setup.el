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

(defun java-indent-buffer ()
  (interactive)
  ;; (unless (executable-find "astyle")
  ;;   (error "Command astyle is not available"))
  (save-current-line-column
   (let ((file +buffer-indent-temporary-filename+))
     (write-region (point-min) (point-max) file)
     (erase-buffer)
     (shell-command
      (join-lines (list (concat +emacs-config-path+ "/tmp/astyle.custom")
                        "--style=java"
                        "--indent=spaces=4"
                        "--brackets=attach"
                        "--pad-oper"
                        "--pad-header"
                        "--unpad-paren"
                        "--keep-one-line-statements"
                        "--keep-one-line-blocks"
                        "--convert-tabs"
                        "--align-pointer=name"
                        "--mode=java"
                        "--suffix=none"
                        "--lineend=linux"
                        ;; "--indent-namespaces" ;; uncomment to indent toplevel
                        ;;                       ;; classes or interfaces
                        (format "<%s" file))
                  " ")
      (current-buffer)))))

(defun java-setup ()
  (cc-setup :define-special-keys nil)
  (setf c-basic-offset 4
        vim:shift-width 4)

  (setup-eproj-symbnav)
  (setup-outline-headers :header-start "/"
                         :header-symbol "*"
                         :length-min 3
                         :length-max 9))

(provide 'java-setup)

;; Local Variables:
;; End:

;; java-setup.el ends here
