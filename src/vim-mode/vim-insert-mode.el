;; vim-insert-mode.el - VIM insert-mode. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010, 2011 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; TODO :
;; - for some reason GNU Emacs does not show '-- REPLACE --'

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'vim-motions)
  (defvar vim--last-undo))

(require 'vim-macs)
(require 'vim-core)
(require 'vim-undo)
(require 'vim-normal-mode)
(require 'vim-motions)

(defvar-local vim--last-insert-undo nil)

(defvar-local vim--insert-count nil
  "The number of times the insertion should be repeated.")

(defvar-local vim--insert-marker nil
  "A marker which is placed at the point where insertion started.")

(defvar vim-insert-mode-on-exit nil
  "Execute this when we have left insert mode and fully enabled normal mode.")

(defcustom vim-insert-mode-replace-cursor 'hbar
  "Cursor for replace-mode."
  :group 'vim-cursors
  :type 'symbolp)

(vim-define-keymap insert-mode "insert-mode" :map-command imap)

(vim-define-mode insert "VIM insert-mode"
  :ident "I"
  ;; :message "-- INSERT --"
  :keymaps '(vim-insert-mode-keymap)
  :command-function #'vim-insert-mode-command
  :cursor 'bar)

(add-hook 'vim-insert-mode-on-hook #'vim--insert-mode-activated)
(add-hook 'vim-insert-mode-off-hook #'vim--insert-mode-deactivated)

(vim-defcmd vim:insert-mode-toggle-replace ()
  "Toggles overwrite-mode in insert-mode."
  (unless (vim-insert-mode-p)
    (error "Toggling overwrite-mode only allowed in insert-mode"))
  (overwrite-mode nil)
  (if overwrite-mode
      (progn
        (vim-notify "-- REPLACE --")
        (setq cursor-type vim-insert-mode-replace-cursor))
    (progn
      (vim-notify "-- INSERT --")
      (setq cursor-type vim-insert-mode-cursor))))

(defvar-local vim--insert-newline nil)

(defun vim--insert-mode-insert-newline! ()
  "Inserts a newline according to current insert-mode direction."
  (pcase vim--insert-newline
    (`above
     (vim:motion-beginning-of-line:wrapper)
     (insert-char ?\n)
     (forward-line -1)
     (indent-according-to-mode))
    (`below
     (vim:motion-end-of-line:wrapper)
     (insert-char ?\n)
     (indent-according-to-mode))))

(defun vim-start-insert-mode (&optional count newline)
  "Activates insert-mode with a certain repeat `count'.
`newline' should be 'above or 'below or nil which determines
where to insert a newline."
  (setq vim--insert-count count
        vim--insert-newline newline)
  (vim--insert-mode-insert-newline!)
  (when (eq vim--insert-newline 'above)
    (setq vim--insert-newline 'below))
  (when vim--insert-count
    (setq vim--insert-marker (if (eobp) 'eob (copy-marker (1+ (point))))))
  (vim-activate-insert-mode))

(defun vim-insert-mode-command (command)
  "Executes a simple command in insert mode."
  (pcase (vim--cmd-type command)
    (`simple  (vim--insert-execute-simple-command command))
    (`complex (error "No complex command allowed in insert-mode"))
    (_        (vim--insert-execute-motion command))))

(defun vim--insert-execute-simple-command (command)
  "Executes a simple command."
  (vim--execute-simple-command-impl command nil))

(defun vim--insert-execute-motion (command)
  "Executes a motion."
  (vim--execute-motion-impl command))

(defun vim--insert-mode-activated ()
  "Called when insert-mode is activated."
  (overwrite-mode -1)
  (setq vim--last-insert-undo vim--last-undo)
  (add-hook 'pre-command-hook #'vim--insert-save-key-sequence))

(defun vim--insert-mode-deactivated ()
  "Called when insert-mode is deactivated."
  (overwrite-mode -1)
  ;; TODO: how do we use result of this call?
  (vim-set-mark ?^)
  (remove-hook 'pre-command-hook #'vim--insert-save-key-sequence)
  (vim--append-repeat-events! vim--current-key-sequence)
  (setq vim--last-undo vim--last-insert-undo)

  ;; Repeat insertion.
  (when vim--insert-count
    (let ((current-key-sequence (vim--reify-events-no-escape vim--current-key-sequence)))
      (dotimes (_ (1- vim--insert-count))
        (goto-char (if (eq vim--insert-marker 'eob)
                       (point-max)
                     (1- vim--insert-marker)))
        (vim--insert-mode-insert-newline!)
        (execute-kbd-macro current-key-sequence)))
    (when (markerp vim--insert-marker)
      (move-marker vim--insert-marker nil))
    (setq vim--insert-marker nil
          vim--insert-count nil)))

(defun vim--insert-save-key-sequence ()
  "Called in insert-mode to save key-events."
  (when (and (not (eq this-command 'vim-intercept-ESC))
             (functionp this-command))
    (vim--remember-this-command-keys!)))

;; This needs to be a special command because we want to execute
;; ‘vim:insert-mode-exit--impl’ with normal-mode’s command function so
;; that we get point adjustments and repeat events tracking.
(defun vim-insert-mode-exit ()
  (interactive)
  (vim-activate-normal-mode)
  (vim:insert-mode-exit--impl:interactive)
  (when vim-insert-mode-on-exit
    (funcall vim-insert-mode-on-exit)))

(vim-defcmd vim:insert-mode-exit--impl (nonrepeatable)
  "Deactivates insert-mode, returning to normal-mode."
  (goto-char (max (line-beginning-position)
                  (cond ((eq? vim--insert-mode-exit-move-point
                              'dont-move-at-line-end)
                         (if (= (point) (line-end-position))
                           (point)
                           (1- (point))))
                        (vim--insert-mode-exit-move-point
                         (1- (point)))
                        (t
                         (point))))))

(provide 'vim-insert-mode)

;; Local Variables:
;; End:

;; vim-insert-mode.el ends here
