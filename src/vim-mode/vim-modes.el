;; vim-modes.el - Implementation of VIM submodes. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'vim-macs)

(defgroup vim-cursors nil
  "Cursor types for modes."
  :group 'vim-mode)

(defvar-local vim:mode-string nil)

(defvar-local vim:active-mode nil
  "The currently active vim-mode.")

(defvar-local vim:active-command-function nil
  "The command function of the currently active vim-mode.")

(defvar vim:mode-alist nil
  "Assocative list of all registered submodes, (mode-symbol . mode-text).")

(defvar-local vim:emulation-mode-alist nil
  "List of all keymaps used by some modes.")


(defun vim:update-mode-line (ident)
  "Updates the mode-line to show the specified identifier `ident'."
  ;; Not used, so avoid unnecessary updates. Still, may come handy someday.
  ;; (setq vim:mode-string (concat "<" (or ident "?") ">"))
  ;; (force-mode-line-update)
  )


(defun vim:mode-name (mode)
  "Converts a mode-name to vim-mode naming conventions, e.g.
'normal is converted to 'vim:normal-mode."
  (string->symbol (concat "vim:" (symbol->string mode) "-mode")))


(defun vim:activate-mode (mode)
  "Activates a certain vim-mode, disabling the currently active one."
  (when vim:active-mode
    (funcall vim:active-mode -1))
  (when mode
    (funcall (vim:mode-name mode) 1)))


(defmacro vim:set-keymaps (vim-mode-name keymaps)
  "Does setting up of keymaps for the current mode."
  (when (eq (car-safe vim-mode-name) 'quote)
    (setq vim-mode-name (cadr vim-mode-name)))
  (when (eq (car-safe keymaps) 'quote)
    (setq keymaps (cadr keymaps)))
  `(setq vim:emulation-mode-alist
         (list
          ,@(cons '(cons 'vim:intercept-ESC-mode vim:intercept-ESC-keymap)
                  (mapcan (lambda (keym)
                            (let ((localname
                                   (string->symbol
                                    (replace-regexp-in-string
                                     "mode-keymap" "mode-local-keymap"
                                     (symbol->string keym)))))
                              (if (eq localname keym)
                                (list `(cons ',vim-mode-name ,keym))
                                (list `(cons ',vim-mode-name ,localname)
                                      `(cons ',vim-mode-name ,keym)))))
                          keymaps)))))

(defun vim:default-command-function (&rest args)
  (error "Default noop command function called with args %s" args))

(defmacro* vim:define-mode (name doc
                                 &rest body
                                 &key
                                 ident
                                 message
                                 command-function
                                 (cursor ''box)
                                 keymaps)
  "Defines a new VIM-mode with certain `name', mode-line-identifier `ident',
activation `message', a `command-function' to be called when a
vim-command should be executed, a `cursor' shape and a list of `keymaps'."
  (declare (indent 2))
  (let* ((mode-name (vim:mode-name name))
         (pred-name (string->symbol (concat (symbol->string mode-name) "-p")))
         (on-name (string->symbol (concat "vim:activate-" (symbol->string name) "-mode")))
         (cursor-name (string->symbol (concat (symbol->string mode-name)
                                              "-cursor")))
         (update-keymaps-func-name (string->symbol
                                    (concat (symbol->string mode-name)
                                            "-update-keymaps"))))
    `(progn
       (defcustom ,cursor-name ,cursor
         ,(concat "The cursor-type for vim-mode " (symbol->string name) ".")
         :group 'vim-cursors)

       (push (cons ',mode-name ,(symbol->string name)) vim:mode-alist)

       ;; (add-hook 'find-file-hook 'vim:normal-mode-update-keymaps)
       (define-minor-mode ,mode-name ,doc nil nil nil
         (when ,mode-name
           ,@(when ident `((vim:update-mode-line ,ident)))
           ,@(when message `((let (message-log-max) (message ,message))))
           (setq vim:active-mode ',mode-name)
           (setq vim:active-command-function
                 ,(if command-function
                    command-function
                    #'vim:default-command-function))
           (vim:set-cursor ,cursor-name)
           (,update-keymaps-func-name))
         ,@(progn
             (while (keywordp (car body)) (pop body) (pop body))
             body))

       (defun ,pred-name ()
         ,(concat "Returns t iff vim-mode is in " (symbol->string name) " mode.")
         (and ,mode-name t))

       (defun ,on-name ()
         ,(concat "Activates " (symbol->string name) " mode.")
         (interactive)
         (vim:activate-mode ',name))

       (defun ,update-keymaps-func-name ()
         "This function should be called after setting up local keymaps."
         (vim:set-keymaps ',mode-name ,keymaps)))))

(font-lock-add-keywords 'emacs-lisp-mode '("vim:define-mode"))


(provide 'vim-modes)

;; Local Variables:
;; End:

;; vim-modes.el ends here
