;;; sql-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 20 January 2012
;; Keywords:
;; Requirements:
;; Status:


(eval-after-load "sql"
                 '(progn
                   (load-library "sql-indent")))

(defun sql-setup ()
  (init-common)

  (setf vim:normal-mode-local-keymap           (make-keymap)
        vim:visual-mode-local-keymap           (make-keymap)
        vim:insert-mode-local-keymap           (make-sparse-keymap)
        vim:motion-mode-local-keymap           (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap))

  (def-keys-for-map2 vim:normal-mode-lockal-keymap
    ("<f9>" sql-send-buffer))

  (def-keys-for-map2 (vim:normal-mode-lockal-keymap
                      vim:visual-mode-local-keymap)
    ("g n" sql-beginning-of-statement)
    ("g t" sql-end-of-statement))

  (def-keys-for-map2 vim:visual-mode-local-keymap
    ("<f1>" sql-send-region)
    ("<f9>" sql-send-region)))

(add-hook 'sql-mode-hook #'sql-setup)

(provide 'sql-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; sql-setup.el ends here
