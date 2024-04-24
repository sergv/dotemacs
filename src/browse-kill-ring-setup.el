;; kill-ring-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  1 March 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'macro-util))

(require 'browse-kill-ring)

(setf browse-kill-ring-display-duplicates nil
      browse-kill-ring-maximum-display-length nil)

;;;###autoload
(defun browse-kill-ring ()
  (interactive)
  (browse-kill-ring-start-for-variable 'kill-ring "*Kill Ring*"))

;;;###autoload
(defun browse-eshell-input-history ()
  (interactive)
  (browse-kill-ring-start-for-variable 'eshell-history-ring "*Eshell History*"))

;;;###autoload
(defun browse-comint-input-history ()
  (interactive)
  (browse-kill-ring-start-for-variable 'comint-input-ring "*Comint Input History*"))

;;;###autoload
(defun browse-kill-ring-mode-setup ()
  (hl-line-mode +1)
  (setq-local mode-line-format
              '(" %[%b%] "
                (:eval (when buffer-read-only
                         "(RO)"))
                ("("
                 mode-name
                 (:eval (format "[%d items]"
                                (generic/length (browse-kill-ring--get-ring-value))))
                 ")")
                (:eval
                 (when (buffer-narrowed-p)
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
    ("<return>" browse-kill-ring-insert-and-quit)

    ("/"        browse-kill-ring-search-forward)
    ("?"        browse-kill-ring-search-backward)
    ("k"        browse-kill-ring-undo-tree-undo-other-window)
    ("K"        browse-kill-ring-undo-tree-redo-other-window)

    ("u"        browse-kill-ring-search-repeat)
    ("U"        browse-kill-ring-search-repeat-opposite-direction)

    ("<escape>" browse-kill-ring-quit)))

;;;###autoload
(add-hook 'browse-kill-ring-mode-hook #'browse-kill-ring-mode-setup)

(provide 'browse-kill-ring-setup)

;; Local Variables:
;; End:

;; kill-ring-setup.el ends here
