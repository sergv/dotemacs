;; set-up-platform.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 25 July 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(defun %emacs-boot--string-trim-whitespace (str)
  "Trim whitespaces from string"
  (replace-regexp-in-string "^[ \t\n\r\v\f]+\\|[ \t\n\r\v\f]+$" "" str))

(defun %emacs-boot--string->symbol (str)
  (intern str))

(defvar +platform+ nil
  "List of the form (<os> <use> <misc>), <misc> is optional,
<os> may be 'linux or 'windows.
Use may be 'home, 'asus-netbook, 'netbook, 'work or something other
Range of platforms may be expanded (extended?) in the future.")

(let* ((system-type-file-dirs
        (cond ((eq system-type 'windows-nt)
               '("~"))
              ((memq system-type
                     '(gnu gnu/linux gnu/kfreebsd darwin))
               '("/home/sergey" "~"))
              (t
               '("~"))))
       (system-type-file
        (find-if (lambda (file)
                   (and file
                        (file-exists-p file)
                        (file-executable-p file)))
                 (mapcan (lambda (prefix)
                           (list (concat prefix "/system_type.sh")
                                 (when (eq system-type 'windows-nt)
                                   (concat prefix "/system_type.cmd"))))
                         system-type-file-dirs))))
  (setf +platform+
        (cond
          ((not (null system-type-file))
           (read
            (with-temp-buffer
              (call-process system-type-file
                            nil
                            (current-buffer))
              (%emacs-boot--string-trim-whitespace
               (buffer-substring-no-properties (point-min)
                                               (point-max))))))
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


(defun platform-os-type? (os-type)
  (assert (and (listp +platform+) (not (null +platform+))))
  (eq (car +platform+) os-type))

(defun platform-use? (use)
  "Use may be a symbol or a list of symbols"
  (cond ((symbolp use)
         (eq (cadr +platform+) use))
        ((listp use)
         (some (lambda (u) (eq (cadr +platform+) u)) use))
        (t
         (error "invalid use argument: %s" use))))

(defun platform-dependent-root ()
  "Retrieve platform-dependent filesystem root for current combination of
platform OS and usage."
  (cond ((platform-os-type? 'windows)
         (expand-file-name "~"))
        ((platform-os-type? 'linux)
         (expand-file-name "~"))
        (t
         ;; fallback to make it work in unanticipated scenarios
         (expand-file-name "~"))))

(defun platform-dependent-executable (exec-name)
  "Return EXEC-NAME, which must be a file name, transformed according to
conventions of platform this emacs instance is currently running on (
e.g. add .exe if running on windows).

Note: this function should not be applied for scripts, only for native
binaries."
  (let ((final-name (if (platform-os-type? 'windows)
                      (concat exec-name ".exe")
                      exec-name)))
    (cond ((not (file-exists-p final-name))
           (message "Executable file %s does not exist" final-name))
          ((not (file-executable-p final-name))
           (message "Executable file %s does not have executable permissions" final-name))
          (t
           final-name))))

(provide 'set-up-platform)

;; Local Variables:
;; End:

;; set-up-platform.el ends here
