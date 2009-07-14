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


(vim:deflocalvar vim:current-node
  "The currently active node."
  nil)

(vim:deflocalvar vim:current-key-sequence
  "The key-sequence of the current command."
  nil)

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
      (catch 'vim:unknown-command
        (condition-case err
            (vim:input-key last-command-event)
          (error
           (vim:reset-key-state)
           (error err)))
        nil)
    (if (and (vim:mode-default-handler vim:active-mode)
                 (funcall (vim:mode-default-handler vim:active-mode)))
        (vim:reset-key-state)
      (unwind-protect
          (vim:escape-to-emacs vim:current-key-sequence)
        (vim:reset-key-state)))))


;; from viper
(defsubst vim:ESC-event-p (event)
  (let ((ESC-keys '(?\e (control \[) escape))
        (key (viper-event-key event)))
    (member key ESC-keys)))


;; from viper
(defun vim:escape-to-emacs (events)
  "Executes some `events' in emacs."
        
  (let* ((vim-key-mode nil)
         (unread-command-events events)
         (keys (read-key-sequence nil))
         (event (elt (listify-key-sequence keys) 0)))

    (when (vim:ESC-event-p event)
      (let ((unread-command-events keys))
        (setq keys (read-key-sequence nil))))
        
    (let ((command (key-binding keys)))
      (setq this-command command)
      (setq last-command-event (elt keys (1- (length keys))))
      (setq last-command-char last-command-event)
      (command-execute command)
      (when (memq command '(digit-argument
                            universal-argument))
        (vim:escape-to-emacs nil)))))


(defun vim:enable-keymap ()
  (when (and vim-mode
             (not (memq this-command '(vim:handle-key
                                       digit-argument
                                       universal-argument
                                       universal-argument-other-key))))
    (remove-hook 'post-command-hook 'vim:enable-keymap)
    (setq vim-key-mode t)))


(defvar vim:mode-map nil)
(setq vim:mode-map
      (list 'keymap
            (cons t 'vim:handle-key)))


(define-minor-mode vim-key-mode
  "VIM emulation mode - keymap"
  :lighter nil
  :initial-value nil
  :global nil
  :keymap vim:mode-map)

