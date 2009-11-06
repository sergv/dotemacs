;;; vim-modes.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.2.0
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

(provide 'vim-modes)

(vim:deflocalvar vim:mode-string)
(defun vim:update-mode-line (ident)
  "Updates the mode-line to show the specified identifier `ident'."
  (setq vim:mode-string (concat "<" (or ident "?") ">"))
  (force-mode-line-update))


(defun vim:mode-name (mode)
  "Converts a mode-name to vim-mode naming conventions, e.g.
'normal is converted to 'vim:normal-mode."
  (intern (concat "vim:" (symbol-name mode) "-mode")))

(vim:deflocalvar vim:active-mode nil
  "The currently active vim-mode.") 

(vim:deflocalvar vim:active-command-function nil
  "The command function of the currently active vim-mode.")

(defun vim:activate-mode (mode)
  "Activates a certain vim-mode, disabling the currently active one."
  (when vim:active-mode
    (funcall vim:active-mode -1))
  (when mode
    (funcall (vim:mode-name mode) 1)))


(defmacro* vim:define-mode (name doc
                                 &key
                                 ident
                                 keymap
                                 command-function
                                 (cursor ''box)
                                 activate
                                 deactivate
                                 )
  "Defines a new VIM-mode with certain `name', mode-line-identifiert `ident',
a `keymap' and a `command-function' to be called when a vim-command should
be executed."
  (let* ((mode-name (vim:mode-name name))
         (pred-name (intern (concat (symbol-name mode-name) "-p")))
         (on-name (intern (concat "vim:activate-" (symbol-name name) "-mode")))
         (cursor-name (intern (concat (symbol-name mode-name)
                                      "-cursor"))))
    `(progn
       (defcustom ,cursor-name ,cursor
         ,(concat "The cursor-type for vim-mode " (symbol-name name) ".")
         :group 'vim-mode)
       
       (define-minor-mode ,mode-name ,doc
         :keymap nil
         :init-value nil
         
         (if ,mode-name
             (progn
               ,@(when ident `((vim:update-mode-line ,ident)))
               (setq vim:active-mode ',mode-name)
               (setq vim:active-command-function
                     ,(if command-function
                          command-function
                        'vim:default-command-function))
               (setq cursor-type ,cursor-name)
               ,@(and activate `((funcall ,activate))))
           (progn
             ,@(and deactivate `((funcall ,deactivate))))))

       (add-to-list 'vim:emulation-mode-alist (cons ',mode-name ,keymap))

       (defun ,pred-name ()
         ,(concat "Returns t iff vim-mode is in " (symbol-name name) " mode.")
         (and ,mode-name t))
       (defun ,on-name ()
         ,(concat "Activates " (symbol-name name) " mode.")
         (interactive)
         (vim:activate-mode ',name)))))
