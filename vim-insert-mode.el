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

(defvar vim:insert-mode-keymap (vim:make-node))

(defvar vim:next-insert-undo nil)
(defvar vim:last-insert-undo nil)
(defvar vim:current-insert-key-sequence nil)

(defun vim:insert-mode-activate ()
  (message "-- INSERT --")
  (setq overwrite-mode nil)
  (setq cursor-type 'bar)
  (setq vim:current-insert-key-sequence vim:current-key-sequence)
  (setq vim:last-insert-undo (or vim:next-insert-undo vim:last-undo)))

(defun vim:insert-mode-deactivate ()
  (setq cursor-type 'box)
  (setq overwrite-mode nil)
  (setq vim:last-undo vim:last-insert-undo)
  (setq vim:last-insert-undo nil)
  (unless executing-kbd-macro
    (setq vim:repeat-events (vconcat (reverse vim:current-insert-key-sequence) [escape]))
    (setq vim:current-insert-key-sequence nil))
  )

(defun vim:insert-mode-default-handler ()
  "The default event handler of the insert mode."
  (unless executing-kbd-macro
    (push last-command-event vim:current-insert-key-sequence))
  nil)

(vim:define vim:insert-mode-exit (count)
  (vim:activate-mode vim:normal-mode)
  (goto-char (max (line-beginning-position) (1- (point)))))

(defvar vim:insert-mode
  (vim:make-mode :name "Insert"
                 :activate #'vim:insert-mode-activate
                 :deactivate #'vim:insert-mode-deactivate
                 :execute-command #'vim:default-mode-exec-cmd
                 :execute-motion #'vim:default-mode-exec-motion
                 :default-handler #'vim:insert-mode-default-handler
                 :keymap 'vim:insert-mode-keymap))


(defun vim:replace-mode-activate ()
  (message "-- REPLACE --")
  (setq cursor-type 'hbar)
  (setq vim:current-insert-key-sequence vim:current-key-sequence)
  (setq overwrite-mode t)
  (setq vim:last-insert-undo (or vim:next-insert-undo vim:last-undo)))

(defvar vim:replace-mode
  (vim:make-mode :name "Replace"
                 :activate #'vim:replace-mode-activate
                 :deactivate #'vim:insert-mode-deactivate
                 :execute-command #'vim:default-mode-exec-cmd
                 :execute-motion #'vim:default-mode-exec-motion
                 :default-handler #'vim:insert-mode-default-handler
                 :keymap 'vim:insert-mode-keymap))
