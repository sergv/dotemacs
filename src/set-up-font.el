;; set-up-font.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday,  7 November 2012
;; Description:

(eval-when-compile
  (require 'cl-lib))

(when dump-mode
  (error "Should not be reached when dumping"))

(require 'set-up-platform)
(require 'set-up-environment-variables)
(require 'set-up-paths)

(defun font-exist? (font)
  "Test whethef FONT is avaliable on the system."
  (condition-case nil
      (if (null (x-list-fonts font)) nil t)
    (error nil)))

(defconst +emacs-fonts+
  (seq-filter
   #'font-exist?
   (append
    '("-*-Iosevka Slab Lig-normal-normal-normal-*-16-*-*-*-*-*-iso10646-1"
      "-*-Iosevka Slab Lig-*-*-*-*-16-*-*-*-*-*-*-*"
      "-*-Iosevka Slab-normal-normal-normal-*-16-*-*-*-*-*-iso10646-1"
      "-*-Terminus-normal-normal-normal-*-16-*-*-*-*-*-iso10646-1"
      "-*-Terminus (TTF)-normal-normal-normal-*-16-*-*-*-*-*-iso10646-1"
      "-raster-Terminus-normal-normal-normal-*-20-*-*-*-c-*-iso8859-1")
    (if (and (<= (display-pixel-width) 1280)
             (<= (display-pixel-height) 1024))
        '("-outline-Anonymous Pro-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1")
      '("-outline-Anonymous Pro-normal-normal-normal-mono-19-*-*-*-c-*-iso8859-1"))
    '("-unknown-Droid Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"
      "-unknown-DejaVu Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"
      "-unknown-Inconsolata-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1"
      "-unknown-Liberation Mono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"
      "-unknown-FreeMono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1"
      "-monotype-Courier New-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")))
  "List of fonts from best to worst availabse on the system.")

(defvar current-font (car +emacs-fonts+))

(defvar current-font-scaling nil)

(defun get-default-font-scaling ()
  "Come up with a reasonable font scaling based on screen resolution."
  (let* ((width (display-pixel-width))
         (height (display-pixel-height)))
    (cond
      ((and (= 3840 width)
            (= 2160 height))
       120)
      ((and (= 1366 width)
            (= 768 height))
       120)
      ((and (= 1024 width)
            (= 600 height))
       90)
      (t
       120))))

(unless noninteractive
  (defun set-up-font--init-font-and-scaling ()
    (let ((scaling (or current-font-scaling
                       (get-default-font-scaling)))
          (frames (frame-list)))
      (setf current-font-scaling scaling)
      (set-frame-font current-font nil frames)
      (dolist (frame frames)
        (set-face-attribute 'default frame :height scaling))))

  (defun set-up-font--set-current-font-for-frame (&option frame)
    (cl-assert (font-exist? current-font) nil "Font does not exist: %s" current-font)
    (set-frame-font current-font nil (if frame (list frame) nil)))

  (add-hook 'window-setup-hook #'set-up-font--init-font-and-scaling)
  (add-hook 'after-make-frame-functions #'set-up-font--set-current-font-for-frame))

;;;###autoload
(defun update-font-scaling (&optional new-scaling)
  "Set up font scaling for current frame. If NEW-SCALING is specified then
use that, otherwise either use past specified value or a reasonable default."
  (interactive "P")
  (let ((effective-scaling
         (or new-scaling
             current-font-scaling
             (get-default-font-scaling))))
    (setf current-font-scaling effective-scaling)
    (dolist (frame (frame-list))
      (set-face-attribute 'default frame :height effective-scaling))))

;; Set default font for all unicode characters.

(defconst +dejavu-sans-mono-font+ "DejaVu Sans Mono")

(when (member +dejavu-sans-mono-font+ (font-family-list))
  (set-fontset-font t 'unicode +dejavu-sans-mono-font+ nil nil))

(provide 'set-up-font)

;; Local Variables:
;; End:

;; set-up-font.el ends here
