;; set-up-environment-variables.el --- -*- lexical-binding: t -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 27 July 2012
;; Description:

(eval-when-compile (require 'cl))

(require 'set-up-platform)

;;;; Environment variables

(let ((env-loaded (getenv "BASHRC_ENV_LOADED")))
  (if (or (null env-loaded)
          (and (stringp env-loaded)
               (not (string= env-loaded "1"))))
      ;; Load the environment if it wasn't done before by e.g. running from
      ;; terminal with environment set up.
      (let ((env-config-file
             (or (getenv "EMACS_ENV_DEFS")
                 (let ((default (expand-file-name "~/.bash_env")))
                   (message "Using default environment config file, '%s', because EMACS_ENV_DEFS variable not set"
                            default)
                   default))))
        (unless (file-exists-p env-config-file)
          (error "Environment config file '%s' does not exist" env-config-file))
        (save-match-data
          (let ( ;; shell is expected to be a bash shell
                (all-values (shell-command-to-string
                             (format "source '%s' && printenv --null;"
                                     env-config-file))))
            (dolist (entry (split-string all-values "[\0]" t))
              (let ((eq-pos (cl-position ?\= entry)))
                (when eq-pos
                  (let ((var (substring entry 0 eq-pos)))
                    (setenv var
                            (substring entry (+ eq-pos 1))))))))))
    (message "Skipping environment configuration because BASHRC_ENV_LOADED variable is set to 1")))

(cl-defun add-env-var-to-list (env-var list &key (append t))
  (dolist (dir (parse-colon-path (getenv env-var)))
    (add-to-list list dir append)))

(when-windows
 (setenv "HOME" (cygwin-directory-name-to-emacs (getenv "HOME"))))

(add-env-var-to-list "PATH" 'exec-path :append t)

(when (executable-find "cat")
  (setenv "PAGER" "cat"))

(provide 'set-up-environment-variables)

;; Local Variables:
;; End:

;; set-up-environment-variables.el ends here
