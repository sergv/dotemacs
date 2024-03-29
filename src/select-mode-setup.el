;; select-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 14 January 2015
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common)
(require 'keys-def)

;;;###autoload
(autoload 'select-mode "select-mode" nil t)
;;;###autoload
(autoload 'select-mode-start-selection "select-mode")

;;;###autoload
(setf select-mode-restore-windows-configuration-on-hide t)

;;;###autoload
(defun select-mode-setup ()
  (def-keys-for-map select-mode-map
    +vi-essential-keys+
    +vim-search-keys+
    +vim-word-motion-keys+
    +vim-special-keys+

    ("h" select-mode-select-next-item)
    ("t" select-mode-select-previous-item))
  (vim-activate-emacs-mode))

(provide 'select-mode-setup)

;; Local Variables:
;; End:

;; select-mode-setup.el ends here
