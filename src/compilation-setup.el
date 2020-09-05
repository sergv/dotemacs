;; compilation-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 29 February 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'common)
(require 'compile)
(require 'compilation-navigation)
(require 'xterm-color)

(setf compilation-always-kill t
      ;; don't ask - just save
      compilation-ask-about-save nil)

(defvar-local *compilation-jump-error-regexp*
  (rx-let ((delim (or "/"
                      "\\"))
           (filename (+ (not (any ?/ ?\\ ?\n ?\t)))))
    (rx bol
        (group (? delim)
               (* filename
                  delim)
               filename)
        ":"
        (group (+ (any (?0 . ?9))))
        ":"
        (group (+ (any (?0 . ?9))))
        ":"))
  "Regexp which is used by `compilation-jump-to-next-error'
and `compilation-jump-to-prev-error' to detect errors
in compilation or related buffers.")


(defun compilation--apply-ansi-colours-filter (f proc string)
  "Turn ANSI colour codes into colourful text!"
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'compilation--apply-ansi-colours-filter)

;;;###autoload
(defun compilation-jump-to-next-error ()
  "Jump to next error in the compilation buffer."
  (interactive)
  (unless (compilation-buffer-p (current-buffer))
    (error "Not in a compilation buffer"))
  (circular-jump-forward *compilation-jump-error-regexp* nil))

;;;###autoload
(defun compilation-jump-to-prev-error ()
  "Jump to previous error in the compilation buffer."
  (interactive)
  (unless (compilation-buffer-p (current-buffer))
    (error "Not in a compilation buffer"))
  (circular-jump-backward *compilation-jump-error-regexp* nil))

;;; compilation info

;; (cl-defstruct (compilation-caller-info
;;                (:conc-name compilation-caller-info/))
;;   mode   ;; Mode from which compilation was started.
;;   compile-command
;;   buffer ;; Buffer from where compilation was started.
;;   )
;;
;; (defparameter *compile-caller-info* nil
;;   "Structure containing information about buffer, major mode etc.
;; from where current compile command was invoked. Should be cleared
;; up by functions in compilation-finish-functions.")
;;
;; (defadvice compilation-start (before
;;                               compilation-start-store-info
;;                               activate
;;                               compile)
;;   "Record information about caller of compile command into
;; `*compile-caller-info*'"
;;   (setf *compile-caller-info*
;;         (make-compilation-caller-info
;;          :mode major-mode
;;          :compile-command compile-command
;;          :buffer (current-buffer))))


(eval-after-load "compile"
  '(progn
     (def-keys-for-map compilation-mode-map
       +vi-keys+
       +vim-special-keys+
       +vim-search-keys+
       +vim-mock:word-motion-keys+
       ("<up>"     compilation-jump-to-prev-error)
       ("<down>"   compilation-jump-to-next-error)
       ("t"        compilation-jump-to-prev-error)
       ("h"        compilation-jump-to-next-error)
       ("M-p"      nil)
       ("q"        remove-buffer)
       ("C-c C-c"  kill-compilation)
       ("m"        vim-mock:motion-jump-item)
       ("0"        vim-mock:motion-beginning-of-line-or-digit-argument)
       ("^"        vim-mock:motion-first-non-blank)
       ("$"        vim-mock:motion-end-of-line)

       ("C-v"      set-mark-command)
       ("C-y"      copy-region-as-kill)
       ("v"        set-mark-command)
       ("y"        copy-region-as-kill)

       (("C-m" "<f9>" "H") recompile)
       ("<return>"     compilation/goto-error)
       ("SPC"          compilation/goto-error-other-window))

     (def-keys-for-map compilation-button-map
       ("C-m" nil))))

(defun compilation-mode-setup ()
  (hl-line-mode +1))

(add-hook 'compilation-mode-hook #'compilation-mode-setup)

(vim:defcmd vim:recompile (nonrepeatable)
  (recompile))

(provide 'compilation-setup)

;; Local Variables:
;; End:

;; compilation-setup.el ends here
