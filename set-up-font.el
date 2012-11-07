;; set-up-font.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday,  7 November 2012
;; Description:

(require 'set-up-platform)
(require 'set-up-environment-variables)
(require 'set-up-paths)

(defun font-exist? (font)
  (condition-case nil
      (if (null (x-list-fonts font)) nil t)
    (error nil)))

(defvar *emacs-font* nil
  "Font currently being used")

(cond ((and (platform-os-type? 'linux)
            (platform-use? '(home asus-netbook)))
       (setf *emacs-font*
             "-unknown-Anonymous Pro-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1"))
      ((and (platform-os-type? 'linux)
            (platform-use? 'netbook))
       (setf *emacs-font*
             "-unknown-Anonymous Pro-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"))
      ((and (platform-os-type? 'windows)
            (platform-use? 'work))
       (let ((win-anonymous-pro "-outline-Anonymous Pro-normal-normal-normal-mono-19-*-*-*-c-*-iso8859-1")
             (win-courier-new "-outline-Courier New-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"))
         (cond ((font-exist? win-anonymous-pro)
                (setf *emacs-font* win-anonymous-pro))
               ((font-exist? win-courier-new)
                (setf *emacs-font* win-courier-new))))))

(when *emacs-font*
  (set-frame-font *emacs-font*)

  (add-hook
   'after-make-frame-functions
   (lambda (new-frame)
     (with-current-frame new-frame
       (set-frame-font *emacs-font*)))))


(provide 'set-up-font)

;; Local Variables:
;; End:

;; set-up-font.el ends here
