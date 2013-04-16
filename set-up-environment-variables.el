;; set-up-environment-variables.el --- -*- lexical-binding: t -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 27 July 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'set-up-platform)


(when (file-exists-p (concat (platform-dependent-root) "/.bash_env"))
  (let* ((variables
          (concat "PATH LD_LIBRARY_PATH PYTHONPATH EDITOR "
                  "INFOPATH MANPATH PKG_CONFIG_PATH "
                  "JAVA_HOME ANDROID_HOME ANDROID_SDK SDK_ROOT SDK_HOME NDK_HOME ANDROID_NDK "
                  "LEIN_JAVA_CMD "
                  "CCACHE_BASEDIR CCACHE_COMPRESS CCACHE_DIR"))
         (values (shell-command-to-string
                  (format ". %s/.bash_env; printenv %s;"
                          (platform-dependent-root)
                          variables))))
    (loop
      for var in (split-string variables " ")
      for value in (split-string values "\n")
      do (setenv var value))))


(defun* env-var-into-list (env-var list &key (append t))
  (mapc (lambda (dir)
          (add-to-list list dir append))
        (parse-colon-path (getenv env-var))))

(env-var-into-list "PATH" 'exec-path :append t)

(when (executable-find "cat")
  (setenv "PAGER" "cat"))

(provide 'set-up-environment-variables)

;; Local Variables:
;; End:

;; set-up-environment-variables.el ends here
