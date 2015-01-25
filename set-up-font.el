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
  "Test whethef FONT is avaliable on the system."
  (condition-case nil
      (if (null (x-list-fonts font)) nil t)
    (error nil)))

(defvar *emacs-font* nil
  "Default font for use everywhere or nil if nothing suitable found.")

(defun set-up-fonts/set-emacs-font-if-exists (font)
  (if (font-exist? font)
    (progn
      (setf *emacs-font* font)
      t)
    nil))


(defconst +emacs-fonts+
  (remove-if-not
   #'font-exist?
   (append
    (cond
      ((and (platform-os-type? 'linux)
            (platform-use? '(home asus-netbook work)))
       '("-xos4-Terminus-normal-normal-normal-*-14-*-*-*-c-80-iso10646-1"
         "-unknown-Terminus (TTF)-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"
         "-unknown-Anonymous Pro-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1"))
      ((and (platform-os-type? 'linux)
            (platform-use? 'netbook))
       '("-xos4-Terminus-normal-normal-normal-*-14-*-*-*-c-80-iso10646-1"
         "-unknown-Terminus (TTF)-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"
         "-unknown-Anonymous Pro-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"))
      ((and (platform-os-type? 'windows)
            (platform-use? 'work))
       (append
        '("-outline-Terminus-normal-normal-normal-mono-16-*-*-*-m-0-iso10646-1"
          "-outline-Terminus (TTF)-normal-normal-normal-mono-16-*-*-*-m-0-iso10646-1")
        (if (and (<= (display-pixel-width) 1280)
                 (<= (display-pixel-height) 1024))
          '("-outline-Anonymous Pro-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1")
          '("-outline-Anonymous Pro-normal-normal-normal-mono-19-*-*-*-c-*-iso8859-1")))))
    '("-unknown-Droid Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"
      "-unknown-DejaVu Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"
      "-unknown-Inconsolata-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1"
      "-unknown-Liberation Mono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"
      "-unknown-FreeMono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1"
      "-monotype-Courier New-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
    ;; this is not the best font and therefore it comes latest
    (when (and (platform-os-type? 'windows)
               (platform-use? 'work))
      '("-outline-Courier New-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"))))
  "List of fonts from best to worst availabse on the system.")

(when (and (not *emacs-font*)
           (not (null +emacs-fonts+)))
  (set-up-fonts/set-emacs-font-if-exists (first +emacs-fonts+)))

(when *emacs-font*
  (set-frame-font *emacs-font*)

  (add-hook 'after-make-frame-functions
            (lambda (new-frame)
              (with-current-frame new-frame
                (set-frame-font *emacs-font*)))))

;; set default font for all unicode characters

(defconst +dejavu-sans-mono-font+ "DejaVu Sans Mono")

(when (member +dejavu-sans-mono-font+ (font-family-list))
  (set-fontset-font t 'unicode +dejavu-sans-mono-font+ nil nil))

(provide 'set-up-font)

;; Local Variables:
;; End:

;; set-up-font.el ends here
