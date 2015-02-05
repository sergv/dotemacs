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

  (def-keys-for-map vim:normal-mode-local-keymap
    ("<f9>" sql-send-buffer)
    ("`"    sql-send-buffer))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    ("g t" sql-beginning-of-statement)
    ("g h" sql-end-of-statement))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("`"    sql-send-region)
    ("<f9>" sql-send-region)))

(add-hook 'sql-mode-hook #'sql-setup)


(provide 'sql-setup)

;; Local Variables:
;; End:

;; sql-setup.el ends here
