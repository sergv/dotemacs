;;; vim-insert-mode.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

(provide 'vim-insert-mode)

(defcustom vim:insert-mode-cursor 'bar
  "The cursor-type for insert-mode."
  :group 'vim-mode)

(defcustom vim:replace-mode-cursor 'hbar
  "The cursor-type for replace-mode."
  :group 'vim-mode)


(defconst vim:insert-mode-keymap (vim:make-node))
(vim:deflocalvar vim:insert-mode-activate-hook nil
  "Hooks called when insert-mode is activated.")
(vim:deflocalvar vim:insert-mode-deactivate-hook nil
  "Hooks called when insert-mode is activated.")

(vim:deflocalvar vim:last-insert-undo nil)
(vim:deflocalvar vim:current-insert-key-sequence nil)

(defun vim:insert-active-p ()
  (if vim:last-insert-undo t nil))

(defun vim:insert-mode-activate ()
  (message "-- INSERT --")
  (setq overwrite-mode nil)
  (setq cursor-type vim:insert-mode-cursor)
  (setq vim:current-insert-key-sequence vim:current-key-sequence)
  (setq vim:last-insert-undo buffer-undo-list))

(defun vim:insert-mode-deactivate ()
  ;; connect the undos
  (when vim:last-insert-undo
    (vim:connect-undos vim:last-insert-undo)
    (setq vim:last-insert-undo nil))
  
  (setq overwrite-mode nil)
  (setq vim:repeat-events (vconcat (reverse vim:current-insert-key-sequence) [escape])))

(defun vim:insert-mode-default-handler ()
  "The default event handler of the insert mode."
  (when (vim:toplevel-execution)
    (push last-command-event vim:current-insert-key-sequence))
  nil)

(vim:define vim:insert-mode-exit ()
            :count nil
  (vim:activate-mode vim:normal-mode)
  (goto-char (max (line-beginning-position) (1- (point)))))

(defconst vim:insert-mode
  (vim:make-mode :name "Insert"
                 :id "I"
                 :activate #'vim:insert-mode-activate
                 :deactivate #'vim:insert-mode-deactivate
                 :execute-command #'vim:default-mode-exec-cmd
                 :execute-motion #'vim:default-mode-exec-motion
                 :default-handler #'vim:insert-mode-default-handler
                 :keymap 'vim:insert-mode-keymap
                 :activate-hook 'vim:insert-mode-activate-hook
                 :deactivate-hook 'vim:insert-mode-deactivate-hook))


(defun vim:replace-mode-activate ()
  (message "-- REPLACE --")
  (setq cursor-type vim:replace-mode-cursor)
  (setq vim:current-insert-key-sequence vim:current-key-sequence)
  (setq overwrite-mode t)
  (setq vim:last-insert-undo buffer-undo-list))

(defconst vim:replace-mode
  (vim:make-mode :name "Replace"
                 :id "R"
                 :activate #'vim:replace-mode-activate
                 :deactivate #'vim:insert-mode-deactivate
                 :execute-command #'vim:default-mode-exec-cmd
                 :execute-motion #'vim:default-mode-exec-motion
                 :default-handler #'vim:insert-mode-default-handler
                 :keymap 'vim:insert-mode-keymap
                 :activate-hook 'vim:insert-mode-activate-hook
                 :deactivate-hook 'vim:insert-mode-deactivate-hook))
