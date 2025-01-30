;; set-up-font.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday,  7 November 2012
;; Description:

(eval-when-compile
  (require 'cl))

(defvar dumping)

(when (or dumping
          dump-mode)
  (error "Should not be reached when dumping"))

(require 'set-up-platform)
(require 'set-up-environment-variables)
(require 'set-up-paths)

(defconst current-font
  (let ((name "Iosevka Slab Lig"))
    (if-let ((iosevka-slab-lig-reg (find-font (font-spec :name name :weight 'regular :slant 'normal)))
             (iosevka-slab-lig-bold (find-font (font-spec :name name :weight 'bold :slant 'normal)))
             (iosevka-slab-lig-reg-it  (find-font (font-spec :name name :weight 'regular :slant 'italic)))
             (iosevka-slab-lig-bold-it (find-font (font-spec :name name :weight 'bold :slant 'italic))))
        iosevka-slab-lig-reg
      (car
       (seq-filter
        (lambda (font)
          (condition-case nil
              (and (x-list-fonts font) t)
            (error nil)))
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
           "-monotype-Courier New-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")))))))

(defvar current-font-scaling nil)

(defun get-default-font-scaling ()
  "Come up with a reasonable font scaling based on screen resolution."
  (let* ((width (display-pixel-width))
         (height (display-pixel-height)))
    (cond
      ((and (<= 5120 width)
            (= 2880 height))
       140)
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

  (defun set-up-font--set-current-font-for-frame (&optional frame)
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

(when (pretty-ligatures-supported?)
  ;; Compose ASCII with harfbuzz to take advantage of the TXTR feature (requires custom
  ;; emacs build as well).
  ;; See also:
  ;; ! https://github.com/mickeynp/ligature.el/issues/53#issuecomment-1828732077
  ;; https://github.com/githubnext/monaspace/issues/12
  ;; https://github.com/harfbuzz/harfbuzz/discussions/4490
  (let ((composition-spec
         `([,(rx (+ (any
                     ?\s ?\! ?\" ?\' ?\( ?\) ?\, ?\- ?\. ?\: ?\; ?\< ?\> ?\? ?\@ ?\[ ?\] ?\_ ?\{ ?\| ?\}
                     (?a . ?z)
                     (?A . ?Z)))
                 ;; Somehow newline does not work as space does and
                 ;; does not lead to glyph enlargement.
                 (? (any ?\r ?\n)))
            0
            font-shape-gstring
            ])))
    (dolist (c '(?\s
                 ?\! ?\" ?\' ?\( ?\) ?\, ?\- ?\. ?\: ?\; ?\< ?\> ?\? ?\@ ?\[ ?\] ?\_ ?\{ ?\| ?\}
                 (?a . ?z)
                 (?A . ?Z)))
      (if (numberp c)
          (set-char-table-range composition-function-table c composition-spec)
        (cl-loop
         for i from (car c) to (cdr c)
         do
         (set-char-table-range composition-function-table i composition-spec)
         )))))

(provide 'set-up-font)

;; Local Variables:
;; End:

;; set-up-font.el ends here
