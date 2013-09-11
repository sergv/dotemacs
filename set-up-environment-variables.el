;; set-up-environment-variables.el --- -*- lexical-binding: t -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 27 July 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'set-up-platform)


(let ((env-config-file (concat (platform-dependent-root) "/.bash_env")))
  (when (file-exists-p env-config-file)
    (save-match-data
      (let* ((variables
              '("PATH"
                "LD_LIBRARY_PATH"
                "PYTHONPATH"
                "EDITOR"
                "INFOPATH"
                "MANPATH"
                "PKG_CONFIG_PATH"
                "JAVA_HOME"
                "ANDROID_HOME"
                "ANDROID_SDK"
                "SDK_ROOT"
                "SDK_HOME"
                "NDK_HOME"
                "ANDROID_NDK"
                "LEIN_JAVA_CMD"
                "CCACHE_BASEDIR"
                "CCACHE_COMPRESS"
                "CCACHE_DIR"))
             ;; shell is expected to be a bash shell
             (all-values (shell-command-to-string
                          (format ". %s; printenv --null;"
                                  env-config-file))))
        (dolist (entry (split-string all-values "[\0]" t))
          (let ((eq-pos (cl-position ?\= entry)))
            (when eq-pos
              (let ((var (substring entry 0 eq-pos)))
                (when (member var variables)
                  (setenv var
                          (substring entry (+ eq-pos 1))))))))))))


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
