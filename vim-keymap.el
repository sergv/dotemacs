;;; vim-keymap.el - Basic keymapping for vim-mode

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(defun* vim:map (keys command &key (keymap nil))
  "Maps the sequence of events `keys' to a `command' in a certain
`keymap.'"
  (define-key keymap keys command))

(defun vim:make-keymap (&optional parent)
  "Creates a new keymap with a certain `parent' keymap."
  (let ((kmap (make-sparse-keymap)))
    (when parent (set-keymap-parent kmap parent))
    kmap))

(defmacro vim:define-keymap (name
                             doc
                             &key
                             map-command)
  "Defines global and local keymaps for a mode with name
vim:`name'-[local-]keymap and a map command vim:`map-command'
and vim:local-`map-command'."
  (let ((glbkeym (concat "vim:" (symbol-name name) "-keymap"))
        (lockeym (concat "vim:" (symbol-name name) "-local-keymap")))
    `(progn
       (defconst ,(intern glbkeym) (vim:make-keymap)
         ,(concat "VIM global keymap: " doc))
       (defconst ,(intern lockeym) (vim:make-keymap)
         ,(concat "VIM buffer local keymap: " doc))
       ,@(when map-command
          `((defsubst ,(intern (concat "vim:" (symbol-name map-command)))
              (keys command)
              ,(concat "Maps the sequence of events `keys' to a `command' in keymap "
                       glbkeym)
              (vim:map keys command :keymap ,(intern glbkeym)))
            (defsubst ,(intern (concat "vim:local-" (symbol-name map-command)))
               (keys command)
               ,(concat "Maps the sequence of events `keys' to a `command' in keymap "
                        lockeym)
               (vim:map keys command :keymap ,(intern lockeym))))))))
(font-lock-add-keywords 'emacs-lisp-mode '("vim:define-keymap"))
             
    
  

;; Interception of ESC event. The ESC event is intercepted. If not
;; followed by another key, i.e. not used as a prefix-key, the event
;; [escape] is sent, otherwise the interception-keymap is disabled for
;; the next command and the ESC event is resent.
(defcustom vim:intercept-ESC-timeout 0.1
  "Time in seconds to wait for another key after an ESC event."
  :group 'vim-mode)

(defconst vim:intercept-ESC-keymap (make-sparse-keymap)
  "Keymap to map ESC to [escape].")

(define-minor-mode vim:intercept-ESC-mode
  "VIM minor mode to capture ESC."
  nil nil nil)

;; This function is defined in vim:compat.el
;; (defun vim:intercept-ESC () ...)

(defun vim:enable-intercept-ESC ()
  "Enables interception of ESC after executing a (prefix-)command."
  (unless (eq this-command 'vim:intercept-ESC)
    (remove-hook 'pre-command-hook 'vim:enable-intercept-ESC)
    (vim:intercept-ESC-mode 1)))

;; Catch '\e' and convert it to [escape] if not used as prefix key.
(vim:map (kbd "ESC") 'vim:intercept-ESC :keymap vim:intercept-ESC-keymap)


;; The override keymap, useful especially in normal-mode.
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


;; This function sets up the keymaps for the current mode.
(defun vim:set-keymaps (mode-name keymaps)
  (setq vim:emulation-mode-alist
        (mapcan #'(lambda (keym)
                    (let ((localname (intern (replace-regexp-in-string "mode-keymap" "mode-local-keymap"
                                                                        (symbol-name keym)))))
                      (if (eq localname keym)
                          (list (cons mode-name (symbol-value keym)))
                        (list (cons mode-name (symbol-value localname))
                              (cons mode-name (symbol-value keym))))))
                keymaps))
  (push (cons 'vim:intercept-ESC-mode vim:intercept-ESC-keymap) vim:emulation-mode-alist))

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

(provide 'vim-keymap)

;;; vim-keymap.el ends here
