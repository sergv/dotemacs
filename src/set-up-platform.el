;; set-up-platform.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 25 July 2012
;; Description:

(eval-when-compile (require 'cl-lib))

;;;; Platform

(defvar +platform+ nil
  "List of the form (<os> <use> <misc>), <misc> is optional,
<os> may be 'linux or 'windows.
Use may be 'home, 'asus-netbook, 'netbook, 'work or something other
Range of platforms may be expanded (extended?) in the future.")

(let* ((system-type-file-dirs
        (cond ((eq system-type 'windows-nt)
               (list (expand-file-name "~")))
              ((memq system-type
                     '(gnu gnu/linux gnu/kfreebsd darwin))
               (list "/home/sergey"
                     (expand-file-name "~")))
              (t
               (list (expand-file-name "~")))))
       (system-type-file
        (find-if (lambda (file)
                   (and file
                        (file-readable-p file)))
                 (mapcar (lambda (prefix)
                           (concat prefix "/system-type.el"))
                         system-type-file-dirs))))
  (setf +platform+
        (cond
          (system-type-file
           (with-temp-buffer
             (insert-file-contents-literally system-type-file)
             (goto-char (point-min))
             (read (current-buffer))))
          ((eq system-type 'windows-nt)
           '(windows work))
          ((eq system-type 'gnu/linux)
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

(defun platform-use? (use)
  "Use may be a symbol or a list of symbols"
  (cond ((symbolp use)
         (eq (cadr +platform+) use))
        ((listp use)
         (some (lambda (u) (eq (cadr +platform+) u)) use))
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
       (message "Executable file %s does not exist" final-name))
      ((not (file-executable-p final-name))
       (message "Executable file %s does not have executable permissions" final-name))
      (t
       final-name))))


(provide 'set-up-platform)

;; Local Variables:
;; End:

;; set-up-platform.el ends here
