;;; vim.el --- a VIM-emulation for Emacs

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"
;; Compatibility: Developed on Emacs 22 - everything else is unknown.
;;
;; URL: http://www.tu-chemnitz.de/~fifr/repos/vim-mode.el
;; 
;; This file is not part of GNU Emacs.


;; simple VIM-mode for Emacs
;;
;; this project is in a VERY early development state and many function
;; have not been implemented yet.
;;
;; If you want to try, open this file in your Emacs and evaluate the buffer.
;; The mode can be activated by 'M-x vim-mode'.
;;
;; Don't forget to disable Viper if you want to try vim-mode.
;;
;;
;; Documentation:
;;
;; The project is divided into many files.  Each file implements some
;; almost-independent feature:
;;
;;  - vim.el:  This file just sets up the mode and loads the other files.
;;
;;  - vim-keys.el: This one of the most important files.  The core of
;;                 all key-handling is done here.  Here you find the
;;                 heart of all magic stuff happening in vim-mode.
;;
;;  - vim-node.el: This is second most important file.  vim-mode has
;;                 its own keymap.  The implementation of the
;;                 structure is in this file.
;;
;;  - vim-vim.el: This file contains the magic coupling (parsed) key-events
;;                to actual motions and commands.  Furthermore it contains
;;                the framework to define new motions and commands.
;;
;;  - vim-modes.el: Each VIM-mode (normal-mode, insert-mode, ...) is
;;                  represented by some functions and variables.  The
;;                  corresponding data-structures are in this file.   
;;
;;  - vim-insert-mode.el: The implementation of insert-mode.         
;;                                                                    
;;  - vim-normal-mode.el: The implementation of normal-mode.         
;;                                                                    
;;  - vim-visual-mode.el: The implementation of visual-mode.         
;;                                                                    
;;  - vim-commands.el: The implementations of commands like 'delete', 
;;                     'yank', 'paste' and so on.               
;;
;;  - vim-motions.el: The implementations of motion commands like 'h',
;;                    'i', 'j', 'k', 'f', 'w', ...
;;
;;  - vim-undo.el: Some variables and functions for undo/redo.
;;
;;  - vim-maps.el: The definition of the basic keymaps.  This file
;;                 connects the keymaps with the commands and motions
;;                 defined in vim-commands.el and vim-motions.el.
;;
;; TODO:
;;
;; HAVE:
;;   - framework for keymaps, motions, commands and command-mappings
;;   - insert-mode, normal-mode and visual-mode
;;   - simple motions
;;   - deletion, yank, paste, change, replace
;;   - undo/redo
;;   - repeat
;;
;; MISSING:
;;   - better Emacs integration (modes, buffer local variables, ...)
;;   - word motions
;;   - search motions
;;   - text objects
;;   - several commands
;;   - scrolling
;;   - ex-mode
;;   - marks and register
;;   - repeating based on commands instead of key-sequences?

(require 'cl)

(defmacro vim:deflocalvar (name &rest args)
  `(progn
     (defvar ,name ,@args)
     (make-variable-buffer-local ',name)))

(let ((load-path (cons (expand-file-name ".") load-path)))
                (load "vim-node")
  (load "vim-vim")
  (load "vim-keys")
  (load "vim-modes")
  (load "vim-insert-mode")
  (load "vim-normal-mode")
  (load "vim-visual-mode")
  (load "vim-commands")
  (load "vim-motions")
  (load "vim-undo")
  (load "vim-maps"))

(define-minor-mode vim-mode
  "VIM emulation mode."
  :lighter " VIM"
  :init-value nil
  :global nil
  :keymap nil)

(defun vim:initialize ()
  (if vim-mode
      (progn
	(setq vim-key-mode t)
	(vim:reset-key-state)
	(vim:activate-mode vim:normal-mode))
    (progn
      (setq vim-key-mode nil))))

(add-hook 'vim-mode-hook 'vim:initialize)

