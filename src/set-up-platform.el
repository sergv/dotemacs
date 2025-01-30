;; set-up-platform.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 25 July 2012
;; Description:

(eval-when-compile (require 'cl))

(defvar current-font)

;;;; Platform

(defvar +platform+ nil
  "List of the form (<os> <use> <misc>), <misc> is optional,
<os> may be 'linux or 'windows.
Use may be 'home, 'asus-netbook, 'netbook, 'work or something other
Range of platforms may be expanded (extended?) in the future.")

(let ((sys-type-env (getenv "EMACS_SYSTEM_TYPE")))
  (setf +platform+
        (cond
          (sys-type-env
           (read sys-type-env))
          ((eq system-type 'windows-nt)
           '(windows work))
          ((memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
           '(linux home))
          (t
           '(linux home)))))

(unless (and (listp +platform+)
             (memq (car +platform+)
                   '(linux windows)))
  (error "+platform+'s os %s should be one of 'linux or 'windows"
         (car +platform+)))

(defmacro fold-platform-os-type (on-linux on-windows)
  (let ((os-type (car +platform+)))
    (cond
      ((eq os-type 'linux)
       on-linux)
      ((eq os-type 'windows)
       on-windows)
      (t
       (error "Invalid platform os type: %s" os-type)))))

(defmacro when-windows (&rest body)
  (let ((os-type (car +platform+)))
    (when (eq os-type 'windows)
      `(progn
         ,@body))))

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

(defun pretty-ligatures-supported? ()
  (and (bound-and-true-p current-font)
       (cond
         ((stringp current-font)
          (string-match-p "Iosevka Slab Lig" current-font))
         ((fontp current-font)
          (string-match-p "Iosevka Slab Lig" (font-get current-font :name)))
         (t
          (error "Invalid current font: %s" current-font)))))

(provide 'set-up-platform)

;; Local Variables:
;; End:

;; set-up-platform.el ends here
