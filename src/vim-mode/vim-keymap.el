;; vim-keymap.el - Basic keymapping for vim-mode --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'macro-util))

(require 'common-small)

(defmacro vim--kbdmacro-to-command (events)
  "Creates a command passing prefix-argument to given keyboard-macro `events'."
  (let ((arg '#:arg))
    `(lambda (,arg)
       (interactive "P")
       (execute-kbd-macro
        (if ,arg
          (vconcat (number-to-string (prefix-numeric-value ,arg))
                   ,events)
          ,events)))))

(cl-defun vim--def-key (keys command &key (keymap nil))
  "Maps the sequence of events `keys' to a `command' in a certain
keymap. `keymap' may be the keymap itself or a symbol denoting
the variable where the keymap is stored. If the variable contains
`nil' a new keymap is created."
  (when (and (symbolp keymap))
    (unless (symbol-value keymap)
      (setf (symbol-value keymap) (vim--make-keymap)))
    (setq keymap (symbol-value keymap)))
  (if (or (stringp command)
          (vectorp command))
    (let ((kbdevents command))
      (define-key keymap keys (vim--kbdmacro-to-command kbdevents)))
    (define-key keymap keys command)))

(defun vim--make-keymap (&optional parent)
  "Creates a new keymap with a certain `parent' keymap."
  (let ((kmap (make-sparse-keymap)))
    (when parent (set-keymap-parent kmap parent))
    kmap))

(cl-defmacro vim-define-keymap (name
                                doc
                                &key
                                map-command)
  "Defines global and local keymaps for a mode with name
vim-`name'-[local-]keymap and a map command vim-`map-command'
and vim-local-`map-command'."
  (declare (indent 2))
  (let ((global (concat "vim-" (symbol->string name) "-keymap"))
        (local (concat "vim-" (symbol->string name) "-local-keymap")))
    `(progn
       (defconst ,(string->symbol global) (vim--make-keymap)
         ,(concat "VIM global keymap: " doc))
       (defvar-local ,(string->symbol local) nil
         ,(concat "VIM buffer local keymap: " doc))
       ,@(when map-command
           `((defsubst ,(string->symbol (concat "vim-" (symbol->string map-command)))
               (keys command)
               ,(concat "Maps the sequence of events `keys' to a `command' in keymap "
                        global)
               (vim--def-key keys command :keymap ',(string->symbol global)))
             (defsubst ,(string->symbol (concat "vim-local-" (symbol->string map-command)))
               (keys command)
               ,(concat "Maps the sequence of events `keys' to a `command' in keymap "
                        local)
               (vim--def-key keys command :keymap ',(string->symbol local))))))))

;; Interception of ESC event. The ESC event is intercepted. If not
;; followed by another key, i.e. not used as a prefix-key, the event
;; [escape] is sent, otherwise the interception-keymap is disabled for
;; the next command and the ESC event is resent.
(defcustom vim-intercept-ESC-timeout 0.1
  "Time in seconds to wait for another key after an ESC event."
  :group 'vim-mode-general)

(defconst vim-intercept-ESC-keymap (make-sparse-keymap)
  "Keymap to map ESC to [escape].")

(define-minor-mode vim-intercept-ESC-mode
  "VIM minor mode to capture ESC."
  :init-value nil
  :lighter nil
  :keymap nil)

(defun vim-enable-intercept-ESC ()
  "Enables interception of ESC after executing a (prefix-)command."
  (unless (eq this-command 'vim-intercept-ESC)
    (remove-hook 'pre-command-hook 'vim-enable-intercept-ESC)
    (vim-intercept-ESC-mode 1)))

;; Catch '\e' and convert it to [escape] if not used as prefix key.
(vim--def-key (kbd "ESC") 'vim-intercept-ESC :keymap vim-intercept-ESC-keymap)

;; The override keymap, useful especially in normal-mode.
(defconst vim-override-keymap (make-keymap)
  "Global parent keymap to override some Emacs default bindings.")
(suppress-keymap vim-override-keymap)

(provide 'vim-keymap)

;; Local Variables:
;; End:

;; vim-keymap.el ends here
