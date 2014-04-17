;; haskell-outline.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'search)
(require 'yafolding)
(require 'haskell-misc)

(search-def-autoexpand-advices
 (yafolding-show)
 (haskell-mode))

(defun haskell-setup-folding ()
  (setq buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table
                          'selective-display
                          (string-to-vector " ...\n"))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("z C" yafolding-hide-all)
    ("z c" yafolding-hide)
    ("z O" yafolding-show-all)
    ("z o" yafolding-show)
    ("z T" yafolding-toggle-all)))


(provide 'haskell-outline)

;; Local Variables:
;; End:

;; haskell-outline.el ends here
