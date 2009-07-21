;;; vim-keys.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later.
;;
;; This file contains code adapted from viper (viper-util.el).
;; viper-util.el is part of GNU Emacs.

(provide 'vim-keys)


(vim:deflocalvar vim:current-node nil
  "The currently active node.")

(vim:deflocalvar vim:current-key-sequence nil
  "The key-sequence of the current command.")

(vim:deflocalvar vim:new-buffer nil
  "The buffer the be made current at the end of key-handline.")

(defun vim:reset-key-state ()
  (setq vim:current-node (vim:active-keymap))
  (when (vim:toplevel-execution)
    (setq vim:current-key-sequence nil)))

(defun vim:input-key (key)
  "Appends the given key to the current command."
  
  (let ((node (vim:get-subnode vim:current-node key)))
    (if node
        (vim:execute-node key node)
      (vim:continue-node key))))


(defun vim:continue-node (key)
  "Called if the current keymap does not contain key, tries to continue the command with next-keymap."

  ;; call the last node's function
  (when (vim:node-function vim:current-node)
    (funcall (vim:node-function vim:current-node) vim:current-node))
  
  ;; continue with the next keymap
  (let ((node (if (vim:node-next-keymap vim:current-node)
                  (vim:get-subnode (vim:node-next-keymap vim:current-node) key)
                nil)))
    (if node
        (vim:execute-node key node)
      (throw 'vim:unknown-command 2))))


(defun vim:execute-node (key node)
  "Execute the command associated with node."

   ;; If this command has no continuation keymap, call its function.
   (if (and (null (vim:node-next-keymap node))
            (vim:node-function node))
       (funcall (vim:node-function node) node)
     (vim:go-to-node node)))


(defun vim:go-to-node (node)
  "Changes the current node."
  (setq vim:current-node node))


(defun vim:handle-key ()
  (interactive)
  (when (vim:toplevel-execution)
    (push last-command-event vim:current-key-sequence))
  (when 
      (or (null vim:active-mode)
	  (catch 'vim:unknown-command
	    (condition-case err
                (let ((vim:new-buffer nil))
                  (vim:input-key (if (consp last-command-event)
                                     (car last-command-event)
                                   last-command-event))
                  (when vim:new-buffer
                    (set-buffer vim:new-buffer)))
	      (error
	       (vim:reset-key-state)
	       (error err)))
	    nil))

    (if (and vim:active-mode
	     (vim:mode-default-handler vim:active-mode)
	     (funcall (vim:mode-default-handler vim:active-mode)))
        (vim:reset-key-state)
      (unwind-protect
          ;; TODO: should we send more than only the current event?
          (vim:escape-to-emacs (list last-command-event))
        (vim:reset-key-state)))))


;; from viper
(defsubst vim:ESC-event-p (event)
  (let ((ESC-keys '(?\e (control \[) escape)))
    (member event ESC-keys)))


;; from viper
(defun vim:escape-to-emacs (events)
  "Executes some `events' in emacs."

  ;; TODO: when executing a command with count in Emacs mode, the
  ;; first event after that command is ignored.
        
  (let (command
        keys
        event)

    ;; read the key-sequence and get the appropriate command
    (let ((vim-local-mode nil))
      (let ((unread-command-events events))
        (setq keys (read-key-sequence nil))
        (setq event (elt (listify-key-sequence keys) 0)))

      (when (vim:ESC-event-p event)
        (let ((unread-command-events keys))
          (setq keys (read-key-sequence nil))))
    
      (setq command (key-binding keys)))
    
    ;; execute the command
    (setq this-command command)
    (setq last-command-event (elt keys (1- (length keys))))
    (setq last-command-char last-command-event)
    (command-execute command)
    (when (memq command '(digit-argument
                          universal-argument))
      (vim:escape-to-emacs nil))))

