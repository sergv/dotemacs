;; set-up-environment-variables.el --- -*- lexical-binding: t -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 27 July 2012
;; Description:

(require 'more-scheme)
(require 'cl)

(when (file-exists-p "~/.bash_env")
  (let* ((variables "PATH LD_LIBRARY_PATH PYTHONPATH EDITOR INFOPATH MANPATH PKG_CONFIG_PATH")
         (values (shell-command-to-string
                  (format ". ~/.bash_env; printenv %s;" variables))))
    (loop
      for var in (split-string variables " ")
      for value in (split-string values "\n")
      do (setenv var value))))

(defun* env-var-into-list (env-var list &key (append t))
  (for-each (lambda (dir)
              (add-to-list list dir append))
            (parse-colon-path (getenv env-var))))

(env-var-into-list "PATH" 'exec-path :append t)


(provide 'set-up-environment-variables)

;; Local Variables:
;; End:

;; set-up-environment-variables.el ends here
