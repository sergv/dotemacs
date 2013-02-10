;; java-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 17 January 2013
;; Description:

(require 'common)
(require 'cc-setup)
(require 'ctags-setup)

(defun java-indent-buffer ()
  (interactive)
  (unless (executable-find "astyle")
    (error "Command astyle is not available"))
  (let ((file +buffer-indent-temporary-filename+))
    (write-region (point-min) (point-max) file)
    (erase-buffer)
    (shell-command
     (mapconcat #'identity
                (list "astyle"
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
                      (format "<%s" file))
                " ")
     (current-buffer))))

(push (cons 'java-mode #'java-indent-buffer) *mode-buffer-indent-function-alist*)

(defun java-setup ()
  (cc-setup :define-special-keys nil)
  (setf c-basic-offset 4
        vim:shift-width 4))

(add-hook 'java-mode-hook #'java-setup)

;; Local Variables:
;; End:

;; java-setup.el ends here
