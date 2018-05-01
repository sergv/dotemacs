;; sql-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 20 January 2012
;; Description:

;;;###autoload
(eval-after-load "sql"
  '(progn
     (load-library "sql-indent")))

;;;###autoload
(defun sql-setup ()
  (init-common :use-whitespace 'tabs-only)

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    (("C-m" "<f9>") sql-send-region))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    ("g t" sql-beginning-of-statement)
    ("g h" sql-end-of-statement))

  (def-keys-for-map vim:visual-mode-local-keymap
    (("C-m" "<f9>") sql-send-region)))

;;;###autoload
(add-hook 'sql-mode-hook #'sql-setup)

(provide 'sql-setup)

;; Local Variables:
;; End:

;; sql-setup.el ends here
