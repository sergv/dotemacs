;; select-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 14 January 2015
;; Description:

(require 'common)
(require 'keys-def)

;;;###autoload
(autoload 'select-mode "select-mode" nil t)
;;;###autoload
(autoload 'select-start-selection "select-mode")

;;;###autoload
(eval-after-load "select-mode"
  '(progn
     (setf select-restore-windows-configuration-on-hide t)))

;;;###autoload
(defun select-mode-setup ()
  (def-keys-for-map select-mode-map
    +vi-essential-keys+
    +vim-search-keys+
    +vim-mock:word-motion-keys+
    +vim-special-keys+

    ("h" select-move-selection-down)
    ("t" select-move-selection-up)))

;;;###autoload
(add-hook 'select-mode-hook #'select-mode-setup)

(provide 'select-mode-setup)

;; Local Variables:
;; End:

;; select-mode-setup.el ends here
