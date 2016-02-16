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
          "Display `kill-ring' items in another buffer."
          t)

(autoload 'browse-comint-input-history
          "browse-kill-ring"
          "Display `comint-input-ring' items in another buffer."
          t)

(autoload 'browse-haskell-interactive-input-history
          "browse-kill-ring"
          "Display `haskell-interactive-mode-history' items in another buffer."
          t)

(eval-after-load "browse-kill-ring"
  '(progn
     (advice-remove 'kill-new #'browse-kill-ring-no-kill-new-duplicates)))

(defun browse-kill-ring-mode-setup ()
  (setq-local mode-line-format
              '(" %[%b%] "
                (:eval (when buffer-read-only
                         "(RO)"))
                ("("
                 mode-name
                 (:eval (format "[%s items]"
                                (generic/length (browse-kill-ring/get-ring-value))))
                 ")")
                (:eval
                 (when (buffer-narrowed?)
                   "(Narrowed)"))))
  (def-keys-for-map browse-kill-ring-mode-map
    +vim-special-keys+
    ("y"        nil)

    (","        browse-kill-ring-delete)
    ("h"        browse-kill-ring-forward)
    ("t"        browse-kill-ring-previous)
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
    ("k"        browse-kill-ring-undo-tree-undo-other-window)
    ("K"        browse-kill-ring-undo-tree-redo-other-window)

    ("u"        browse-kill-ring-search-repeat)
    ("U"        browse-kill-ring-search-repeat-opposite-direction)

    ("<escape>" browse-kill-ring-quit)))

(add-hook 'browse-kill-ring-mode-hook #'browse-kill-ring-mode-setup)

(provide 'browse-kill-ring-setup)

;; Local Variables:
;; End:

;; kill-ring-setup.el ends here
