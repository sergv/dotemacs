;; set-up-platform.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 25 July 2012
;; Description:

(eval-when-compile
  (require 'cl))

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

(let ((system-type-file (concat (cond ((eq system-type 'windows-nt)
                                       "~")
                                      ((memq system-type
                                             '(gnu gnu/linux gnu/kfreebsd darwin))
                                       "/home/sergey")
                                      (t
                                       "~"))
                                "/system_type.sh")))
  (cond
    ((and (file-exists-p system-type-file)
          (file-executable-p system-type-file))
     (setf +platform+ (read
                       (with-temp-buffer
                         (call-process system-type-file
                                       nil
                                       (current-buffer))
                         (%emacs-boot--string-trim-whitespace
                          (buffer-substring-no-properties (point-min)
                                                          (point-max)))))))
    ((eq system-type 'windows-nt)
     (setf +platform+ '(windows nil)))
    (t
     (setf +platform+ '(linux home)))))

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
  (cond ((platform-os-type? 'windows)
         "~")
        ((platform-os-type? 'linux)
         (if (platform-use? '(home netbook asus-netbook))
           "/home/sergey"
           "~"))))


(provide 'set-up-platform)

;; Local Variables:
;; End:

;; set-up-platform.el ends here
