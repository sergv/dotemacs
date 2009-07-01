(provide 'vim-normal-mode)

(defvar vim:normal-mode-keymap (vim:make-node))

(defun vim:normal-mode-activate ()
  )

(defun vim:normal-mode-deactivate ()
  )


(defun vim:normal-insert (count motion)
  (vim:activate-mode vim:insert-mode))

(defun vim:normal-append (count motion)
  (unless (eolp) (forward-char))
  (vim:activate-mode vim:insert-mode))


(defvar vim:normal-mode
  (vim:make-mode :name "Normal"
                 :activate #'vim:normal-mode-activate
                 :deactivate #'vim:normal-mode-deactivate
                 :execute-command #'vim:default-mode-exec-cmd
                 :execute-motion #'vim:default-mode-exec-motion
                 :keymap 'vim:normal-mode-keymap))
