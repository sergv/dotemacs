;;; vim-modes.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

(provide 'vim-modes)

(defstruct (vim:mode
            (:constructor vim:make-mode))
  name               ; The name of the mode.
  activate           ; Called when the mode is activated.
  deactivate         ; Called when the mode is deactivated.
  execute-command    ; Called to execute a command.
  execute-motion     ; Called to execute a motion.
  keymap             ; The root node of the mode's keymap
  default-handler    ; The function called if no matching key could been found.
  )

(defun vim:mode-get-keymap (mode)
  "Returns the keymap of a modes."
  (let ((keymap (vim:mode-keymap mode)))
    (if (symbolp keymap)
        (symbol-value keymap)
      keymap)))


(defvar vim:active-mode nil)

(defun vim:activate-mode (mode)
  "Activates a mode."
  
  (when vim:active-mode
    (funcall (vim:mode-deactivate vim:active-mode)))
  (setq vim:active-mode mode)
  (when vim:active-mode
    (funcall (vim:mode-activate mode)))
  
  (vim:reset-key-state))


(defun vim:active-keymap ()
  (if vim:active-mode
      (vim:mode-get-keymap vim:active-mode)
    nil))
    
(defun vim:default-mode-exec-cmd (cmd count motion &optional arg)
  (if arg
      (funcall (vim:command-function cmd) count motion arg)
    (funcall (vim:command-function cmd) count motion)))

(defun vim:default-mode-exec-motion (motion)
  (goto-char (vim:motion-end motion)))

