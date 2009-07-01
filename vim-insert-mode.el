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

(defun vim:insert-mode-activate ()
  (setq cursor-type 'bar)
  (setq vim:last-insert-undo (or vim:next-insert-undo vim:last-undo)))

(defun vim:insert-mode-deactivate ()
  (setq cursor-type 'box)
  (setq vim:last-undo vim:last-insert-undo
        vim:last-insert-undo nil))

(defun vim:insert-mode-exit ()
  (vim:activate-mode vim:normal-mode)
  (goto-char (max (line-beginning-position) (1- (point)))))

(defvar vim:insert-mode
  (vim:make-mode :name "Insert"
                 :activate #'vim:insert-mode-activate
                 :deactivate #'vim:insert-mode-deactivate
                 :execute-command #'vim:default-mode-exec-cmd
                 :execute-motion #'vim:default-mode-exec-motion
                 :keymap 'vim:insert-mode-keymap))
