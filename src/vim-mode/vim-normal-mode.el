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

(eval-when-compile
  (require 'cl)

  (defvar vim--last-undo))

(require 'vim-defs)
(require 'vim-core)
(require 'vim-keymap)
(require 'vim-compat)
(require 'vim-undo)

;; Basic keymap for motion/scroll commands.
(vim-define-keymap motion-mode "motion mode" :map-command mmap)

(vim-define-mode motion "VIM motion mode\n\n\\{vim-motion-mode-keymap}"
  :ident "M"
  :keymaps '(vim-motion-mode-keymap)
  :command-function #'vim--normal-mode-command)

;; Basic keymap for window commands.
;; (vim-define-keymap window-mode "window mode" :map-command wmap)
;;
;; (vim-define-mode window "VIM window mode"
;;                  :ident "W"
;;                  :keymaps '(vim-window-mode-keymap)
;;                  :command-function #'vim--normal-mode-command)


(defconst vim-operator-repeat-keymap (vim--make-keymap)
  "Keymap to bind the repeat-operator-event.")

(defvar-local vim--operator-repeat-last-event nil
  "The command used to enter operator-pending-mode for commands
like 'dd', 'yy',... .")

(vim-define-keymap operator-pending-mode
    "operator pending mode"
  :map-command omap)


(vim-define-mode operator-pending
    "A mode for reading arguments for vim’s complex commands.\nVIM operator-pending mode\n\nOperator pending mode keymap:\n\\{vim-operator-pending-mode-keymap}\n\nMotion mode keymap:\n\\{vim-motion-mode-keymap}\n\nOperator repeat keymap:\n\\{vim-operator-repeat-keymap}\n\nOverride keymap:\n\\{vim-override-keymap}"
  :ident "O"
  :keymaps '(vim-operator-pending-mode-keymap
             vim-motion-mode-keymap
             vim-operator-repeat-keymap
             vim-override-keymap)
  :command-function #'vim--operator-pending-mode-command)

(add-hook 'vim-operator-pending-mode-hook #'vim--operator-pending-activate)
(add-hook 'vim-operator-pending-mode-off-hook #'vim--operator-pending-deactivate)

(defun vim--operator-pending-activate ()
  (cond
    (vim-operator-pending-mode
     (setf vim--operator-repeat-last-event (vector last-command-event))
     (vim--def-key vim--operator-repeat-last-event #'vim:motion-lines:interactive
                   :keymap vim-operator-repeat-keymap)
     ;; Add hook locally so that if an error occurs and a debugger pops up
     ;; we won’t execute in there and only have effect in the buffer
     ;; the error occured in.
     (add-hook 'post-command-hook #'vim--operator-pending-mode-exit nil t))

    (vim--operator-repeat-last-event
     (vim--def-key vim--operator-repeat-last-event nil :keymap vim-operator-repeat-keymap))))


(defun vim--operator-pending-deactivate ()
  ;; See comment in ‘vim--operator-pending-activate’ about touching local hook value.
  (remove-hook 'post-command-hook #'vim--operator-pending-mode-exit t))

(defun vim-operator-pending-mode-exit ()
  "Exits operator-pending-mode and returns to normal-mode."
  (interactive)
  (vim--operator-pending-mode-exit))

(defun vim--operator-pending-mode-exit ()
  "Exits operator-pending-mode and returns to normal-mode."
  (unless (or (vim--is-cmd-p this-command)
              (memq this-command '(vim-digit-argument
                                   universal-argument-other-key
                                   vim-universal-argument-minus
                                   digit-argument)))
    (vim-activate-normal-mode)))


(defun vim--operator-pending-mode-command (command)
  "Executes a complex command in operator-pending mode."
  (if (memq command '(vim:cmd-force-charwise:interactive
                      vim:cmd-force-linewise:interactive
                      vim:cmd-force-blockwise:interactive))
      (progn
        (vim--remember-this-command-keys!)
        (funcall command))
    (unwind-protect
        (pcase (vim--cmd-type command)
          (`simple  (error "No simple commands allowed in operator-pending mode"))
          (`complex (error "No complex commands allowed in operator-pending mode"))
          (`special (error "No special commands allowed in operator-pending mode"))
          ;; Command type omitted - it’s a motion.
          (_        (vim--normal-execute-complex-command command)))
      (when (vim-operator-pending-mode-p)
        (vim-activate-normal-mode)))))


(vim-defcmd vim:cmd-force-charwise (nonrepeatable)
  "Forces the operator to be characterwise.
If the old motion type was linewise, the motion will become exclusive.
If the old motion type was already characterwise exclusive/inclusive will be toggled."
  (setf vim--current-force-motion-type 'char))


(vim-defcmd vim:cmd-force-linewise (nonrepeatable)
  "Forces the operator to be linewise."
  (setf vim--current-force-motion-type 'linewise))


(vim-defcmd vim:cmd-force-blockwise (nonrepeatable)
  "Forces the operator to be blockwise."
  (setf vim--current-force-motion-type 'block))


(vim-define-keymap normal-mode "normal mode" :map-command nmap)

(vim-define-mode normal "VIM normal mode\n\nNormal mode keymap:\n\\{vim-normal-mode-keymap}\n\nOperator pending mode keymap:\n\\{vim-operator-pending-mode-keymap}\n\nMotion mode keymap:\n\\{vim-motion-mode-keymap}\n\nOverride keymap:\n\\{vim-override-keymap}"
  :ident "N"
  ;; :message "-- NORMAL --"
  :keymaps '(vim-normal-mode-keymap
             vim-operator-pending-mode-keymap
             vim-motion-mode-keymap
             vim-override-keymap)
  :command-function #'vim--normal-mode-command
  :cursor 'hbar)

(defun vim--normal-mode-command (command)
  "Executes a motion or simple-command or prepares a complex command."
  (pcase (vim--cmd-type command)
    (`simple  (vim--normal-execute-simple-command command))
    (`complex (vim--normal-prepare-complex-command command))
    (`special (error "no special so far"))
    (_        (vim--normal-execute-motion command))))


(defun vim--normal-execute-motion (command)
  "Executes a motion."
  (vim--execute-motion-impl command)
  (vim--forget-command-keys!)
  (vim--adjust-point))

(defun vim--execute-motion-impl (command)
  (setf vim--current-motion command)

  (when current-prefix-arg
    (setf vim--current-motion-count (prefix-numeric-value current-prefix-arg)))

  (when (vim--cmd-char-arg-p command)
    (setf vim--current-motion-arg (read-char-exclusive)))

  (vim-execute-current-motion)

  (vim--reset-key-state!))

(defun vim--normal-execute-simple-command (command)
  "Executes a simple command."
  (vim--execute-simple-command-impl command t)
  (vim--adjust-point))

(defun vim--execute-simple-command-impl (command atomic-command-for-keys?)
  "Executes a simple command."
  (when current-prefix-arg
    (setf vim--current-cmd-count (prefix-numeric-value current-prefix-arg)))
  (when (vim--cmd-char-arg-p command)
    (setf vim--current-cmd-arg (read-char-exclusive)))
  (vim--prepare-buffer-undo-list!)
  (let* ((vim--last-undo buffer-undo-list)
         (record-repeat? (and (vim--cmd-repeatable-p command)
                              atomic-command-for-keys?))
         (events (when record-repeat?
                   (vim--remember-this-command-keys!)
                   vim--current-key-sequence)))
    (vim--funcall-save-buffer command
                              nil                    ;; motion
                              vim--current-cmd-count ;; count
                              vim--current-cmd-arg   ;; argument
                              nil                    ;; force
                              vim--current-register  ;; register
                              )
    (when record-repeat?
      (vim--overwrite-repeat-events! events))
    (vim--command-finalize! vim--last-undo atomic-command-for-keys?)))

(defun vim--normal-prepare-complex-command (command)
  "Prepares a complex command, switching to operator-pending mode."
  (when current-prefix-arg
    (setf vim--current-cmd-count (prefix-numeric-value current-prefix-arg)))

  (setf vim--current-cmd command)
  (vim--remember-this-command-keys!)
  (vim-activate-operator-pending-mode))

(defun vim--normal-execute-complex-command (motion-command)
  "Executes a complex command with a certain motion command."
  (setf vim--current-motion motion-command)

  (when current-prefix-arg
    (setf vim--current-motion-count (prefix-numeric-value current-prefix-arg)))

  (when (or vim--current-motion-count vim--current-cmd-count)
    (setf vim--current-motion-count (* (or vim--current-cmd-count 1)
                                       (or vim--current-motion-count 1)))
    (setf vim--current-cmd-count nil))

  (when (vim--cmd-char-arg-p motion-command)
    (setf vim--current-motion-arg (read-char-exclusive)))

  (vim--prepare-buffer-undo-list!)
  (let* ((vim--last-undo buffer-undo-list)
         (repeatable? (vim--cmd-repeatable-p vim--current-cmd))
         (events (when repeatable?
                   (vim--remember-this-command-keys!)
                   vim--current-key-sequence)))
    ;; (vim--funcall-save-buffer vim--current-cmd
    ;;                           :motion (vim--get-current-cmd-motion))
    (vim--funcall-save-buffer vim--current-cmd
                              (vim--get-current-cmd-motion)
                              nil
                              nil
                              nil
                              nil)
    (when repeatable?
      (vim--overwrite-repeat-events! events))
    (vim--command-finalize! vim--last-undo t))

  (vim--adjust-point))

(provide 'vim-normal-mode)

;; Local Variables:
;; End:

;; vim-normal-mode.el ends here
