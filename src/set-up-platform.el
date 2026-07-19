;; set-up-platform.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 25 July 2012
;; Description:

;;;; Platform

(defmacro fold-platform-os-type (on-linux on-windows)
  (let ((os-type (car +platform+)))
    (cond
      ((eq os-type 'linux)
       on-linux)
      ((eq os-type 'windows)
       on-windows)
      (t
       (error "Invalid platform os type: %s" os-type)))))

(defun platform-use? (use)
  "Use may be a symbol or a list of symbols"
  (cond ((symbolp use)
         (eq (cadr +platform+) use))
        ((listp use)
         (memq (cadr +platform+) use))
        (t
         (error "invalid use argument: %s" use))))

(defun platform-dependent-executable (exec-name)
  "Return EXEC-NAME, which must be a file name, transformed according to
conventions of platform this emacs instance is currently running on (
e.g. add .exe if running on windows).

Note: this function should not be applied for scripts, only for native
binaries."
  (let ((final-name
         (fold-platform-os-type
          exec-name
          (concat exec-name ".exe"))))
    (cond
      ((not (file-exists-p final-name))
       ;; (message "Executable file %s does not exist" final-name)
       nil)
      ((not (file-executable-p final-name))
       ;; (message "Executable file %s does not have executable permissions" final-name)
       nil)
      (t
       final-name))))

(defmacro when-emacs-version (version-expr &rest body)
  (declare (indent 1))
  (when (eval `(let ((it ,emacs-major-version)) ,version-expr))
    `(progn ,@body)))

(provide 'set-up-platform)

;; Local Variables:
;; End:

;; set-up-platform.el ends here
