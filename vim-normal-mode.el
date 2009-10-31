;;; vim-normal-mode.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.2.0
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

(provide 'vim-normal-mode)

(defconst vim:operator-repeat-keymap (vim:make-keymap vim:override-keymap)
  "Keymap to bind the repeat-operator-event.")
(defconst vim:operator-pending-mode-keymap (vim:make-keymap vim:operator-repeat-keymap)
  "VIM operator-pending-mode keymap.")
(defun vim:omap (keys command)
  "Defines a new operator-pending-mode mapping."
  (vim:map keys command :keymap vim:operator-pending-mode-keymap))

(vim:define-mode operator-pending "VIM operator-pending mode"
                 :ident "O"
                 :keymap vim:operator-pending-mode-keymap
                 :command-function 'vim:operator-pending-mode-command)

(add-hook 'vim:operator-pending-mode-hook 'vim:set-operator-repeat-key)
(defun vim:set-operator-repeat-key ()
  (if vim:operator-pending-mode
      (vim:map (vector last-command-event) 'vim:motion-lines
               :keymap vim:operator-repeat-keymap)
    (setcdr vim:operator-repeat-keymap (keymap-parent vim:operator-repeat-keymap))))


(defun vim:operator-pending-mode-command (command)
  "Executes a complex command in operator-pending mode."
  (unwind-protect
      (case (vim:cmd-type command)
        ('simple (error "No simple-commands allowed in operator-pending mode."))
        ('complex (error "No complex-commands allowed in operator-pending mode."))
        ('special (error "no special so far"))
        (t (vim:normal-execute-complex-command command)))
    
    (when (vim:operator-pending-mode-p)
      (vim:activate-normal-mode))))


(defconst vim:normal-mode-keymap (vim:make-keymap vim:operator-pending-mode-keymap)
  "VIM normal-mode keymap.")
(defun vim:nmap (keys command)
  "Defines a new normal-mode mapping."
  (vim:map keys command :keymap vim:normal-mode-keymap))

(vim:define-mode normal "VIM normal mode"
                 :ident "N"
                 :keymap vim:normal-mode-keymap
                 :command-function 'vim:normal-mode-command
                 :activate 'vim:normal-mode-activate)

(defun vim:normal-mode-activate ()
  "Called when normal-mode is activated."
  (message "-- NORMAL --"))

(defun vim:normal-mode-command (command)
  "Executes a motion or simple-command or prepares a complex command."
  (case (vim:cmd-type command)
    ('simple (vim:normal-execute-simple-command command))
    ('complex (vim:normal-prepare-complex-command command))
    ('special (error "no special so far"))
    (t (vim:normal-execute-motion command))))


(defun vim:normal-execute-motion (command)
  "Executes a motion."
  (setq vim:current-motion command)

  (when current-prefix-arg
    (setq vim:current-motion-count (prefix-numeric-value current-prefix-arg)))

  (when (vim:cmd-arg-p command)
    (setq vim:current-motion-arg (read-char-exclusive)))

  (unwind-protect
      (vim:execute-current-motion)
    
    (vim:reset-key-state)
    (vim:clear-key-sequence)
    (vim:adjust-point)))


(defun vim:normal-execute-simple-command (command)
  "Executes a simple command."
  (when current-prefix-arg
    (setq vim:current-cmd-count (prefix-numeric-value current-prefix-arg)))
  
  (when (vim:cmd-arg-p command)
    (setq vim:current-cmd-arg (read-char-exclusive)))

  (unwind-protect
      (let ((parameters nil)
            (vim:last-undo buffer-undo-list))
        (when (vim:cmd-count-p command)
          (push vim:current-cmd-count parameters)
          (push :count parameters))
        (when (vim:cmd-arg-p command)
          (push vim:current-cmd-arg parameters)
          (push :argument parameters))
        (vim:apply-save-buffer (vim:cmd-function command) parameters)
        (when (vim:cmd-repeatable-p command)
          (setq vim:repeat-events (vconcat vim:current-key-sequence
                                           (this-command-keys-vector))))
        (vim:connect-undos vim:last-undo))

    (vim:reset-key-state)
    (vim:clear-key-sequence)
    (vim:adjust-point)))
    

(defun vim:normal-prepare-complex-command (command)
  "Prepares a complex command, switching to operator-pending mode."
  (when current-prefix-arg
    (setq vim:current-cmd-count (prefix-numeric-value current-prefix-arg)))
  
  (setq vim:current-cmd command)
  (setq vim:current-key-sequence (vconcat vim:current-key-sequence (this-command-keys-vector)))
  (vim:activate-operator-pending-mode))

(defun vim:normal-execute-complex-command (motion-command)
  "Executes a complex command with a certain motion command."
  (setq vim:current-motion motion-command)

  (when current-prefix-arg
    (setq vim:current-motion-count (prefix-numeric-value current-prefix-arg)))

  (when (or vim:current-motion-count vim:current-cmd-count)
    (setq vim:current-motion-count (* (or vim:current-cmd-count 1)
                                      (or vim:current-motion-count 1)))
    (setq vim:current-cmd-count nil))

  (when (vim:cmd-arg-p motion-command)
    (setq vim:current-motion-arg (read-char-exclusive)))

  (unwind-protect
      (let ((vim:last-undo buffer-undo-list))
        (vim:funcall-save-buffer (vim:cmd-function vim:current-cmd)
                                 :motion (vim:get-current-cmd-motion))
        (when (vim:cmd-repeatable-p vim:current-cmd)
          (setq vim:repeat-events (vconcat vim:current-key-sequence
                                           (this-command-keys-vector))))
        (vim:connect-undos vim:last-undo))
    
    (vim:reset-key-state)
    (vim:clear-key-sequence)
    (vim:adjust-point)))


