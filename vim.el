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
;; URL: http://www.tu-chemnitz.de/~fifr/repos/vim-mode
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
;;  - vim-scroll.el: The implementation of scrolling commands like
;;                   'zz', 'Ctrl-F'.
;;
;;  - vim-window-el: The implementation of window commands like 'C-w s'.
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

;; - the special handling of w and W in operator pending mode could be
;;   implemented in a cleaner way by using a special operator-pending
;;   keymap inheriting the motion-keymap
;;
;; - sim(require 'cl)

(provide 'vim)

(require 'cl)

(defgroup vim-mode nil
  "A VIM emulation mode."
  :group 'emulations)

(defcustom vim:whitelist
  nil
  "*List of major modes in which vim-mode should be enabled."
  :type '(repeat symbol)
  :group 'vim-mode)

(defcustom vim:blacklist
  '(debugger-mode)
  "*List of major modes in which vim-mode should *NOT* be enabled."
  :type '(repeat symbol)
  :group 'vim-mode)


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
  (load "vim-scroll")
  (load "vim-window")
  (load "vim-undo")
  (load "vim-maps"))


(defconst vim:mode-map (list 'keymap (cons t 'vim:handle-key)))
(define-minor-mode vim-local-mode
  "VIM emulation mode."
  :lighter " VIM"
  :init-value nil
  :global nil
  :keymap vim:mode-map

  (unless vim-local-mode
    (setq global-mode-string
          (delq 'vim:mode-string global-mode-string ))))

(define-globalized-minor-mode vim-mode vim-local-mode vim:initialize)


(vim:deflocalvar vim:mode-string)

(defun vim:update-mode-line ()
  "Updates the mode-line to show the current active VIM-mode."
  (setq vim:mode-string
        (concat "<"
                (or (and vim:active-mode
                         (vim:mode-id vim:active-mode))
                    "VIM")
                ">"))
  (force-mode-line-update))


(defun vim:initialize ()
  (unless (minibufferp)
    (when (or (and (null vim:whitelist)
                   (not (member major-mode vim:blacklist)))
              (and vim:whitelist
                   (member major-mode vim:whitelist)))
      (vim-local-mode 1)
      (vim:reset-key-state)
      (vim:activate-mode vim:normal-mode)
      (unless (memq 'vim:mode-string global-mode-string)
        (setq global-mode-string
              (append '("" vim:mode-string) (cdr global-mode-string)))))))

