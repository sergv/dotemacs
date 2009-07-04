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
  
  (vim:vim-reset-key-state))


(defun vim:active-keymap ()
  (if vim:active-mode
      (vim:mode-get-keymap vim:active-mode)
    nil))
    
(defun vim:default-mode-exec-cmd (cmd count motion arg)
  (let ((rest (if (vim:cmd-arg-p cmd) (list arg) nil)))
    (cond
     ((vim:cmd-simple-p cmd) (push count rest))
     ((vim:cmd-complex-p cmd) (push motion rest))
     ((vim:cmd-motion-p cmd) (push count rest)))
    (apply cmd rest)
                
    (when (and vim:current-key-sequence
               (vim:cmd-repeatable-p cmd)
               (not executing-kbd-macro))
      (setq vim:repeat-events
            (vconcat (reverse vim:current-key-sequence))))))

(defun vim:default-mode-exec-motion (motion)
  (goto-char (vim:motion-end motion)))

