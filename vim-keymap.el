;;; vim-keymap.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.2.0
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later.
;;
;; This file contains code adapted from viper (viper-util.el).
;; viper-util.el is part of GNU Emacs.

(provide 'vim-keymap)

(defun* vim:map (keys command &key (keymap nil))
  "Maps the sequence of events `keys' to a `command' in a certain
`keymap.'"
  (define-key keymap keys command))


(defun vim:make-keymap (&optional parent)
  "Creates a new keymap with a certain `parent' keymap."
  (let ((kmap (make-sparse-keymap)))
    (when parent (set-keymap-parent kmap parent))
    kmap))


(defconst vim:override-keymap (make-keymap)
  "Global parent keymap to override some Emacs default bindings.")
(suppress-keymap vim:override-keymap)
(vim:map (kbd "ESC ESC ESC")
         (lambda ()
           "Exits any VIM mode and returns to normal-mode."
           (interactive)
           (vim:activate-normal-mode)
           (ding))
         :keymap vim:override-keymap)


;; TODO: This function is currently empty and serves only as hook for
;; defadvice.
(defun vim:reset-key-state ()
  "Resets the current internal key-state."
  nil)

(vim:deflocalvar vim:current-key-sequence nil
  "The key-sequence of the current command.")

(vim:deflocalvar vim:new-buffer nil
  "The buffer the be made current at the end of key-handline.")

(defun vim:clear-key-sequence ()
  "Clears the internal log of key-sequences."
  (setq vim:current-key-sequence nil))
