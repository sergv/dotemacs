;; vim.el --- a VIM-emulation for Emacs --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010, 2011 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; URL: http://www.emacswiki.org/emacs/VimMode
;; License: GPLv2 or later, as described below under "License"
;; Compatibility: Emacs 22, 23
;; Version: 0.4.0
;; Keywords: emulation, vim
;; Human-Keywords: vim, emacs
;;
;; This file is not part of GNU Emacs.

;;; Acknowledgements:

;; This package contains code from several other packages:
;;
;; - rect-mark.el
;; - viper
;; - vimpulse.el
;; - windmove.el
;;
;; Special thanks go to the authors of those packages.

;;; Commentary:

;; A simple VIM-mode for Emacs
;;
;; This project is in an early development state and many function
;; have not been implemented yet.
;;
;; If you want to try, open this file in your Emacs and evaluate the buffer.
;; The mode can be activated by 'M-x vim-mode'.
;;
;; Don't forget to disable Viper if you want to try vim-mode.
;;
;; The project is divided into many files. Each file implements some
;; almost-independent feature. If you want to learn how to implement
;; new commands or motions, look at the files vim-commands.el and
;; vim-motions.el.
;;
;; Here is a short description of the contents of each file:
;;
;;  - vim.el:  This file just sets up the mode and loads the other files.
;;
;;  - vim-compat.el: Compatibility layer for different Emacsen.
;;
;;  - vim-keymap.el: A few functions for defining keymaps for vim-mode.
;;
;;  - vim-macs.el: This file contains the macros for defining motions
;;                and commands.
;;
;;  - vim-defs.el: Global variables.
;;
;;  - vim-core.el: Controlling of active modes and execution of
;;                 motions and commands.
;;
;;  - vim-modes.el: Each VIM-mode (normal-mode, insert-mode, ...) corresponds
;;                  to an Emacs-minor-mode. This file contains some macros and
;;                  functions to define new vim-modes in this context.
;;
;;  - vim-insert-mode.el: The implementation of insert-mode.
;;
;;  - vim-normal-mode.el: The implementation of normal-mode.
;;
;;  - vim-visual-mode.el: The implementation of visual-mode.
;;
;;  - vim-ex.el: The implementation of ex-mode.
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
;;  - vim-ex-commands.el: The implementations of commands like ':edit'
;;                        or ':buffer'.
;;
;;  - vim-search.el: The implementation of '/' and ':substitute'.
;;
;;  - vim-undo.el: Some variables and functions for undo/redo.
;;
;;  - vim-maps.el: The definition of the basic keymaps.  This file
;;                 connects the keymaps with the commands and motions
;;                 defined in vim-commands.el and vim-motions.el.

;;; Changelog:

;; version 0.5
;;     * add ]p and ]P commands
;;     * paste-pop works for all mixes of block/linewise/char and for
;;       paste-before and paste-behind
;;     * add :setmode ex-command for setting vim-mode's start-mode
;;       for the current major-mode
;;     * enable search commands /, ?, *, #, g*, g# in motion-mode,
;;       repeat search with C-n and C-N instead of n and N
;;     * : starts ex-mode in motion-mode
;;     * C-: starts ex-mode in window-mode
;;     * command can take an addition force argument which is set
;;       to non-nil iff an exclamation mark has been typed behind
;;       the command in ex-mode
;;     * ex-commands :bn, :bp
;;     * ex-mode shows info about current command

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'macro-util))

(require 'custom)

(require 'vim-compat)
(require 'vim-core)
(require 'vim-keymap)
(require 'vim-maps)
(require 'vim-normal-mode)

(defgroup vim-mode nil
  "A VIM emulation mode."
  :group 'emulations)

(defgroup vim-mode-general nil
  "General options for Vim-Mode"
  :group 'vim-mode)

(defcustom vim:default-initial-mode
  'normal
  "The default initial vim sub-mode."
  :type '(symbol :tag "vim-mode start mode")
  :group 'vim-mode-general)

(defcustom vim:initial-modes
  (alist->hash-table
   '((Custom-mode . nil)
     (browse-kill-ring-mode . nil)
     (calendar-mode . nil)
     (clojure-compilation-mode . nil)
     (compilation-mode . nil)
     (completion-list-mode . nil)
     (debugger-mode . nil)
     (dired-mode . nil)
     (doc-view-mode . nil)
     (ediff-mode . nil)
     (emms-playlist-mode . nil)
     (flycheck-error-list-mode . nil)
     (fundamental-mode . nil)
     (git-rebase-mode . nil)
     (grep-mode . nil)
     (haskell-compilation-mode . nil)
     (haskell-lint-mode . nil)
     (help-mode . nil)
     (ibuffer-mode . nil)
     (image-mode . nil)
     (isearch-mode . nil)
     (latex-compilation-mode . nil)
     (magit-branch-manager-mode . nil)
     (magit-commit-mode . nil)
     (magit-diff-mode . nil)
     (magit-key-mode . nil)
     (magit-log-mode . nil)
     (magit-popup-mode . nil)
     (magit-popup-sequence-mode . nil)
     (magit-refs-mode . nil)
     (magit-revision-mode . nil)
     (magit-show-branches-mode . nil)
     (magit-stash-mode . nil)
     (magit-status-mode . nil)
     (minimap-mode . nil)
     (occur-mode . nil)
     (org-agenda-mode . nil)
     (select-mode . nil)
     (sldb-mode . nil)
     (slime-fuzzy-completions-mode . nil)
     (slime-inspector-mode . nil)
     (slime-repl-mode . normal)
     (slime-xref-mode . nil)
     (text-mode . normal)
     (undo-tree-visualizer-mode . nil))
   #'eq)
  "Associated list of (major-mode . vim:mode) which specifies the
vim sub-mode in which vim-mode should start when a buffer with the
given major-mode is created."
  :type '(repeat (cons (symbol :tag "major mode") (symbol :tag "vim-mode start mode")))
  :group 'vim-mode-general)


(define-minor-mode vim-local-mode
  "VIM emulation mode."
  :lighter " VIM"
  :init-value nil
  :global nil
  (if vim-local-mode
    (progn
      (vim:initialize-keymaps t))
    (progn
      (vim:initialize-keymaps nil)
      (vim:activate-mode nil))))

(define-globalized-minor-mode vim-mode vim-local-mode vim:initialize)

(defun vim:initialize ()
  (unless (vim:minibuffer-p)
    (awhen (if (hash-table-member-p major-mode vim:initial-modes)
             (gethash major-mode vim:initial-modes nil)
             vim:default-initial-mode)
      (setq vim:active-mode nil)
      (vim-local-mode 1)
      ;; (vim:intercept-ESC-mode 1)
      (vim:activate-mode it))))

(provide 'vim)

;; Local Variables:
;; End:

;; vim.el ends here
