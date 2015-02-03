;; select-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 14 January 2015
;; Description:

(require 'common)
(require 'keys-def)

(autoload 'select-mode "select-mode" nil t)
(autoload 'select-start-selection "select-mode")

(defun select-mode-setup ()
  (def-keys-for-map select-mode-map
    +vi-essential-keys+
    +vi-search-keys+
    +control-x-prefix+
    +vim-word-motion-keys+
    +vim-special-keys+

    ("n" select-move-selection-up)
    ("t" select-move-selection-down)))

(add-hook 'select-mode-hook #'select-mode-setup)

(provide 'select-mode-setup)

;; Local Variables:
;; End:

;; select-mode-setup.el ends here
