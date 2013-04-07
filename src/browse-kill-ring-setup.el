;; kill-ring-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  1 March 2012
;; Keywords:
;; Requirements:
;; Status:

(autoload 'browse-kill-ring
          "browse-kill-ring"
          "Display items in the `kill-ring' in another buffer."
          t)

(autoload 'browse-eshell-input-history
          "browse-kill-ring"
          "Display items in the `eshell-history-ring' in another buffer."
          t)

(autoload 'browse-comint-input-history
          "browse-kill-ring"
          "Display items in the `comint-input-ring' in another buffer."
          t)

(autoload 'browse-nrepl-input-history
          "browse-kill-ring"
          "Display items in the `nrepl-input-history' in another buffer."
          t)



(defun browse-kill-ring-mode-setup ()
  (def-keys-for-map browse-kill-ring-mode-map
    +control-x-prefix+
    +vim-special-keys+
    ("h"        nil)
    ("s"        nil)
    ("y"        nil)

    ("t"        browse-kill-ring-forward)
    ("n"        browse-kill-ring-previous)
    ("<down>"   browse-kill-ring-forward)
    ("<up>"     browse-kill-ring-previous)
    ("r"        browse-kill-ring-update)
    ;; ("p"        browse-kill-ring-insert-move-and-quit)
    ;; ("a"        browse-kill-ring-insert-move-and-quit)
    ;; ("T"        browse-kill-ring-insert-move-and-quit)
    ("o"        browse-kill-ring-occur)
    ;; mnemonic "filter"
    ("f"        browse-kill-ring-occur)
    ("SPC"      browse-kill-ring-insert-move-and-quit)

    ("/"        browse-kill-ring-search-forward)
    ("?"        browse-kill-ring-search-backward)

    ("u"        browse-kill-ring-undo-tree-undo-other-window)
    ("U"        browse-kill-ring-undo-tree-redo-other-window)

    ("<escape>" browse-kill-ring-quit)))

(add-hook 'browse-kill-ring-mode-hook #'browse-kill-ring-mode-setup)

(provide 'browse-kill-ring-setup)

;; Local Variables:
;; End:

;; kill-ring-setup.el ends here
