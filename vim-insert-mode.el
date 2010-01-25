;;; vim-insert-mode.el - VIM insert-mode.

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

(provide 'vim-insert-mode)

(vim:deflocalvar vim:last-insert-undo nil)

(defcustom vim:insert-mode-replace-cursor 'hbar
  "Cursor for replace-mode."
  :group 'vim-mode)

(defconst vim:insert-mode-ESC-keymap (vim:make-keymap)
  "VIM insert-mode keymap for the ESC-key.")
(defconst vim:insert-mode-keymap (vim:make-keymap vim:insert-mode-ESC-keymap)
  "VIM insert-mode keymap.")
(defun vim:imap (keys command)
  "Defines a new insert-mode mapping."
  (vim:map keys command :keymap vim:insert-mode-keymap))

(vim:define-mode insert "VIM insert-mode"
                 :ident "I"
                 :keymap vim:insert-mode-keymap
                 :command-function 'vim:insert-mode-command
                 :cursor 'bar
                 :activate 'vim:insert-mode-activated
                 :deactivate 'vim:insert-mode-deactivated)

(vim:defcmd vim:insert-mode-toggle-replace ()
  "Toggles overwrite-mode in insert-mode."
  (unless (vim:insert-mode-p)
    (error "Toggling overwrite-mode only allowed in insert-mode."))
  (setq overwrite-mode (not overwrite-mode))
  (if overwrite-mode
      (progn
        (let (message-log-max) (message "-- REPLACE --"))
        (setq cursor-type vim:insert-mode-replace-cursor)
        (vim:update-mode-line "R"))
    (progn
      (let (message-log-max) (message "-- INSERT --"))
        (setq cursor-type vim:insert-mode-cursor)
      (vim:update-mode-line "I"))))

(defun vim:insert-mode-command (command)
  "Executes a simple command in insert mode."
  (case (vim:cmd-type command)
    ('simple (vim:normal-execute-simple-command command))
    ('complex (error "No complex command allowed in insert-mode."))
    (t (vim:normal-execute-motion command))))

(defun vim:insert-mode-activated ()
  "Called when insert-mode is activated."
  (setq overwrite-mode nil)
  (let (message-log-max) (message "-- INSERT --"))
  (setq vim:last-insert-undo vim:last-undo)
  (add-hook 'pre-command-hook 'vim:insert-save-key-sequence))
  
(defun vim:insert-mode-deactivated ()
  "Called when insert-mode is deactivated."
  (setq overwrite-mode nil)
  (remove-hook 'pre-command-hook 'vim:insert-save-key-sequence)
  ;; the command that has just ended insert-mode should NOT be repeatable
  ;; and will therefore NOT override repeat-sequence.
  (setq vim:repeat-events (vconcat vim:repeat-events
                                   vim:current-key-sequence))
  (setq vim:last-undo vim:last-insert-undo))

(defun vim:insert-save-key-sequence ()
  "Called in insert-mode to save key-events."
  (when (vim:toplevel-execution)
    (setq vim:current-key-sequence (vconcat vim:current-key-sequence
                                            (vim:this-command-keys)))))

;;; vim-insert-mode.el ends here
