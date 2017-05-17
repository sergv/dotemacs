;; set-up-environment-variables.el --- -*- lexical-binding: t -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 27 July 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'set-up-platform)

(let ((env-loaded (getenv "BASHRC_ENV_LOADED")))
  (when (or (null env-loaded)
            (and (stringp env-loaded)
                 (not (string= env-loaded "1"))))
    ;; Load the environment if it wasn't done before by e.g. running from
    ;; terminal with environment set up.
    (let ((env-config-file (concat (platform-dependent-root) "/.bash_env")))
      (when (file-exists-p env-config-file)
        (save-match-data
          (let ( ;; shell is expected to be a bash shell
                (all-values (shell-command-to-string
                             (format ". %s; printenv --null;"
                                     env-config-file))))
            (dolist (entry (split-string all-values "[\0]" t))
              (let ((eq-pos (cl-position ?\= entry)))
                (when eq-pos
                  (let ((var (substring entry 0 eq-pos)))
                    (setenv var
                            (substring entry (+ eq-pos 1)))))))))))))


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
