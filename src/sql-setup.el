;; sql-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 20 January 2012
;; Description:

(eval-when-compile
  (require 'macro-util))

;;;###autoload
(eval-after-load "sql"
  '(progn
     (load-library "sql-indent")))

(defhydra-derive hydra-sql-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_t_: beginning of statement
_h_: end of statement"
  ("t" sql-beginning-of-statement)
  ("h" sql-end-of-statement))

;;;###autoload
(defun sql-setup ()
  (init-common :use-whitespace 'tabs-only)

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-visual-mode-local-keymap)
    (("C-m" "<f9>") sql-send-region)
    ("g" hydra-sql-vim-normal-g-ext/body)))

;;;###autoload
(add-hook 'sql-mode-hook #'sql-setup)

(provide 'sql-setup)

;; Local Variables:
;; End:

;; sql-setup.el ends here
