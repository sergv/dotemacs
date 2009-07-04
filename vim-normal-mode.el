;;; vim-normal-mode.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

(provide 'vim-normal-mode)

(defvar vim:motion-keymap (vim:make-node))
(defvar vim:normal-mode-keymap
  (vim:make-node :next-keymap vim:motion-keymap))

(defun vim:normal-mode-activate ()
  )

(defun vim:normal-mode-deactivate ()
  )


(defvar vim:normal-mode
  (vim:make-mode :name "Normal"
                 :activate #'vim:normal-mode-activate
                 :deactivate #'vim:normal-mode-deactivate
                 :execute-command #'vim:default-mode-exec-cmd
                 :execute-motion #'vim:default-mode-exec-motion
                 :keymap 'vim:normal-mode-keymap))
