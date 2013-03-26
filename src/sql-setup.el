;; sql-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 20 January 2012
;; Description:


(eval-after-load "sql"
                 '(progn
                   (load-library "sql-indent")))

(defun sql-setup ()
  (init-common)

  (setf vim:normal-mode-local-keymap           (make-keymap)
        vim:visual-mode-local-keymap           (make-sparse-keymap)
        vim:insert-mode-local-keymap           (make-sparse-keymap)
        vim:motion-mode-local-keymap           (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap))

  (def-keys-for-map vim:normal-mode-lockal-keymap
    ("<f9>" sql-send-buffer))

  (def-keys-for-map (vim:normal-mode-lockal-keymap
                     vim:visual-mode-local-keymap)
    ("g n" sql-beginning-of-statement)
    ("g t" sql-end-of-statement))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("<f6>" sql-send-region)
    ("<f9>" sql-send-region)))

(add-hook 'sql-mode-hook #'sql-setup)


(provide 'sql-setup)

;; Local Variables:
;; End:

;; sql-setup.el ends here
