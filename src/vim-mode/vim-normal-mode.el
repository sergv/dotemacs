;; vim-insert-mode.el - VIM normal mode. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.


;;; TODO:
;; - bindings in local-omap keymap will not be seen as motions in
;;   normal-mode since the parent-keymap of the normal-mode keymap
;;   is operator-pending-keymap and not its local counterpart. The
;;   reason is that the binding of the local counterpart will be
;;   changed to a buffer-local binding.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'vim-defs)
(require 'vim-core)
(require 'vim-keymap)
(require 'vim-compat)
(require 'vim-undo)

;; Basic keymap for motion/scroll commands.
(vim:define-keymap motion-mode "motion mode" :map-command mmap)

(vim:define-mode motion "VIM motion mode\n\n\\{vim:motion-mode-keymap}"
  :ident "M"
  :keymaps '(vim:motion-mode-keymap)
  :command-function 'vim:normal-mode-command)

;; Basic keymap for window commands.
;; (vim:define-keymap window-mode "window mode" :map-command wmap)
;;
;; (vim:define-mode window "VIM window mode"
;;                  :ident "W"
;;                  :keymaps '(vim:window-mode-keymap)
;;                  :command-function 'vim:normal-mode-command)


(defconst vim:operator-repeat-keymap (vim:make-keymap)
  "Keymap to bind the repeat-operator-event.")

(defvar-local vim:operator-repeat-last-event nil
  "The command used to enter operator-pending-mode for commands
like 'dd', 'yy',... .")

(defparameter vim:next-command-negated? nil
  "Becomes t if next command should be negated.")

(vim:define-keymap operator-pending-mode
    "operator pending mode"
  :map-command omap)

(vim:define-mode operator-pending
    "VIM operator-pending mode\n\nOperator pending mode keymap:\n\\{vim:operator-pending-mode-keymap}\n\nMotion mode keymap:\n\\{vim:motion-mode-keymap}\n\nOperator repeat keymap:\n\\{vim:operator-repeat-keymap}\n\nOverride keymap:\n\\{vim:override-keymap}"
  :ident "O"
  :keymaps '(vim:operator-pending-mode-keymap
             vim:motion-mode-keymap
             vim:operator-repeat-keymap
             vim:override-keymap)
  :command-function 'vim:operator-pending-mode-command)

(add-hook 'vim:operator-pending-mode-hook 'vim:operator-pending-activate)
(add-hook 'vim:operator-pending-mode-off-hook 'vim:operator-pending-deactivate)

(defun vim:operator-pending-activate ()
  (cond
    (vim:operator-pending-mode
     (setf vim:operator-repeat-last-event (vector last-command-event))
     (vim:map vim:operator-repeat-last-event 'vim:motion-lines
              :keymap vim:operator-repeat-keymap)
     (add-hook 'post-command-hook 'vim:operator-pending-mode-exit))

    (vim:operator-repeat-last-event
     (vim:map vim:operator-repeat-last-event nil :keymap vim:operator-repeat-keymap))))


(defun vim:operator-pending-deactivate ()
  (remove-hook 'post-command-hook 'vim:operator-pending-mode-exit))

(defun vim:operator-pending-mode-exit ()
  "Exits operator-pending-mode and returns to normal-mode."
  (interactive)
  (unless (or (vim:cmd-function this-command)
              (eq this-command 'digit-argument)
              (eq this-command 'universal-argument-other-key))
    (vim:activate-normal-mode)))


(defun vim:operator-pending-mode-command (command)
  "Executes a complex command in operator-pending mode."
  (if (memq command '(vim:cmd-force-charwise
                      vim:cmd-force-linewise
                      vim:cmd-force-blockwise))
    (progn
      (setf vim:current-key-sequence
            (vconcat vim:current-key-sequence (this-command-keys-vector)))
      (funcall command))
    (unwind-protect
        (pcase (vim:cmd-type command)
          (`simple  (error "No simple commands allowed in operator-pending mode"))
          (`complex (error "No complex commands allowed in operator-pending mode"))
          (`special (error "No special commands allowed in operator-pending mode"))
          (_        (vim:normal-execute-complex-command command)))

      (when (vim:operator-pending-mode-p)
        (vim:activate-normal-mode)))))


(vim:defcmd vim:cmd-force-charwise (nonrepeatable)
  "Forces the operator to be characterwise.
If the old motion type was linewise, the motion will become exclusive.
If the old motion type was already characterwise exclusive/inclusive will be toggled."
  (setf vim:current-force-motion-type 'char))


(vim:defcmd vim:cmd-force-linewise (nonrepeatable)
  "Forces the operator to be linewise."
  (setf vim:current-force-motion-type 'linewise))


(vim:defcmd vim:cmd-force-blockwise (nonrepeatable)
  "Forces the operator to be blockwise."
  (setf vim:current-force-motion-type 'block))


(vim:define-keymap normal-mode "normal mode" :map-command nmap)

(vim:define-mode normal "VIM normal mode\n\nNormal mode keymap:\n\\{vim:normal-mode-keymap}\n\nOperator pending mode keymap:\n\\{vim:operator-pending-mode-keymap}\n\nMotion mode keymap:\n\\{vim:motion-mode-keymap}\n\nOverride keymap:\n\\{vim:override-keymap}"
  :ident "N"
  ;; :message "-- NORMAL --"
  :keymaps '(vim:normal-mode-keymap
             vim:operator-pending-mode-keymap
             vim:motion-mode-keymap
             vim:override-keymap)
  :command-function 'vim:normal-mode-command
  :cursor 'box)

(defun vim:normal-mode-command (command)
  "Executes a motion or simple-command or prepares a complex command."
  (pcase (vim:cmd-type command)
    (`simple  (vim:normal-execute-simple-command command))
    (`complex (vim:normal-prepare-complex-command command))
    (`special (error "no special so far"))
    (_        (vim:normal-execute-motion command))))


(defun vim:normal-execute-motion (command)
  "Executes a motion."
  (setf vim:current-motion command)

  (when current-prefix-arg
    (setf vim:current-motion-count (prefix-numeric-value current-prefix-arg)))

  (when (vim:cmd-char-arg-p command)
    (setf vim:current-motion-arg (read-char-exclusive)))

  (vim:execute-current-motion)

  (vim:reset-key-state)
  (vim:clear-key-sequence)
  (vim:adjust-point))


(defun vim:normal-execute-simple-command (command)
  "Executes a simple command."
  (when current-prefix-arg
    (setf vim:current-cmd-count (prefix-numeric-value current-prefix-arg)))
  (when (vim:cmd-char-arg-p command)
    (setf vim:current-cmd-arg (read-char-exclusive)))
  (let* ((parameters nil)
         (vim:last-undo buffer-undo-list)
         (repeatable? (vim:cmd-repeatable-p command))
         (events (if repeatable?
                   (vconcat vim:current-key-sequence
                            (this-command-keys-vector))
                   nil)))
    (when (vim:cmd-count-p command)
      (push vim:current-cmd-count parameters)
      (push :count parameters))
    (when (vim:cmd-char-arg-p command)
      (push vim:current-cmd-arg parameters)
      (push :argument parameters))
    (when (and (vim:cmd-register-p command)
               vim:current-register)
      (push vim:current-register parameters)
      (push :register parameters))
    (vim:apply-save-buffer (vim:cmd-function command) parameters)
    (when repeatable?
      (setf vim:repeat-events events))
    (vim:connect-undos vim:last-undo))
  (vim:reset-key-state)
  (vim:clear-key-sequence)
  (vim:adjust-point))

(defun vim:normal-prepare-complex-command (command)
  "Prepares a complex command, switching to operator-pending mode."
  (when current-prefix-arg
    (setf vim:current-cmd-count (prefix-numeric-value current-prefix-arg)))

  (setf vim:current-cmd command
        vim:current-key-sequence (vconcat vim:current-key-sequence
                                          (this-command-keys-vector)))
  (vim:activate-operator-pending-mode))

(defun vim:normal-execute-complex-command (motion-command)
  "Executes a complex command with a certain motion command."
  (setf vim:current-motion motion-command)

  (when current-prefix-arg
    (setf vim:current-motion-count (prefix-numeric-value current-prefix-arg)))

  (when (or vim:current-motion-count vim:current-cmd-count)
    (setf vim:current-motion-count (* (or vim:current-cmd-count 1)
                                      (or vim:current-motion-count 1)))
    (setf vim:current-cmd-count nil))

  (when (vim:cmd-char-arg-p motion-command)
    (setf vim:current-motion-arg (read-char-exclusive)))

  (let* ((vim:last-undo buffer-undo-list)
         (entry (when vim:complex-command-override-local-keymap
                  (let ((key (concat vim:current-key-sequence (this-command-keys-vector))))
                    (lookup-key vim:complex-command-override-local-keymap
                                key))))
         (repeatable? (vim:cmd-repeatable-p vim:current-cmd))
         (events (if repeatable?
                   (vconcat vim:current-key-sequence
                            (this-command-keys-vector))
                   nil)))
    (if (and entry
             (or (symbol? entry)
                 (functionp entry)))
      (funcall entry)

      (if (and (vim:cmd-register-p vim:current-cmd) vim:current-register)
        (vim:funcall-save-buffer (vim:cmd-function vim:current-cmd)
                                 :motion (vim:get-current-cmd-motion)
                                 :register vim:current-register)
        (vim:funcall-save-buffer (vim:cmd-function vim:current-cmd)
                                 :motion (vim:get-current-cmd-motion))))
    (when repeatable?
      (setf vim:repeat-events events))
    (vim:connect-undos vim:last-undo))

  (vim:reset-key-state)
  (vim:clear-key-sequence)
  (vim:adjust-point))


(provide 'vim-normal-mode)

;; Local Variables:
;; End:

;; vim-normal-mode.el ends here
