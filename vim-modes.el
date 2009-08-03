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
  id                 ; The modeline id character.
  activate           ; Called when the mode is activated.
  deactivate         ; Called when the mode is deactivated.
  execute-command    ; Called to execute a command.
  execute-motion     ; Called to execute a motion.
  keymap             ; The root node of the mode's keymap
  default-handler    ; The function called if no matching key could been found.
  activate-hook      ; hook called after activation
  deactivate-hook    ; hook called after deactivation
  )


(defun vim:mode-get-keymap (mode)
  "Returns the keymap of a modes."
  (let ((keymap (vim:mode-keymap mode)))
    (if (symbolp keymap)
        (symbol-value keymap)
      keymap)))


(vim:deflocalvar vim:active-mode nil
   "The currently active mode.")

(defun vim:activate-mode (mode)
  "Activates a mode."
  
  (when vim:active-mode
    (funcall (vim:mode-deactivate vim:active-mode)))
  
  (let ((last-mode vim:active-mode))
    (setq vim:active-mode mode)
    
    (when (and last-mode
               (vim:mode-deactivate-hook last-mode))
      (run-hooks (vim:mode-deactivate-hook last-mode)))
    
    (when vim:active-mode
      (funcall (vim:mode-activate mode))
      (when (vim:mode-activate-hook vim:active-mode)
        (run-hooks (vim:mode-activate-hook vim:active-mode))))
  
    (vim:update-mode-line)
    (vim:reset-key-state)))


(defun vim:active-keymap ()
  (if vim:active-mode
      (vim:mode-get-keymap vim:active-mode)
    nil))
    
(defun vim:default-mode-exec-cmd (cmd count motion arg)
  (let ((parameters nil))
    (when (vim:cmd-count-p cmd) (push count parameters) (push :count parameters))
    (when (vim:cmd-motion-p cmd) (push motion parameters) (push :motion parameters))
    (when (vim:cmd-arg-p cmd) (push arg parameters) (push :argument parameters))
    (vim:apply-save-buffer cmd parameters)
                
    (when (and vim:current-key-sequence
               (vim:cmd-repeatable-p cmd)
               (not executing-kbd-macro))
      (setq vim:repeat-events
            (vconcat (reverse vim:current-key-sequence))))))

(defun vim:default-mode-exec-motion (motion)
  (if (eq (vim:motion-type motion) 'block)
      (progn
        (goto-line (car (vim:motion-end motion)))
        (move-to-column (cdr (vim:motion-end motion))))
    (goto-char (vim:motion-end motion))))


(defun vim:default-default-handler ()
  "Returns t iff the character is printable."
  ;; TODO:  this is propably not very good

  (if (and (integerp last-command-event)
           (null (event-modifiers last-command-event)))
      (let ((vim-local-mode nil))
        (let ((binding (key-binding (vector last-command-event))))
        (if (eq binding 'self-insert-command)
            (progn
              (ding)
              t)
          nil)))
    nil))
           
      
