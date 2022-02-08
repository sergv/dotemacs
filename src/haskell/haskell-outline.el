;; haskell-outline.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'folding-setup)
(require 'haskell-misc)
(require 'search)
(require 'vim-setup)
(require 'yafolding)

(search-def-autoexpand-advices
 (yafolding-show-element)
 (haskell-mode))

;;;###autoload
(cl-defun haskell-setup-folding (&key (enable-hs-minor-mode t))
  "Configure folding for Haskell. ENABLE-HS-MINOR-MODE controls whether
to enable folding of balanced S-expressions."
  (setup-folding enable-hs-minor-mode '(:header-symbol "-" :length-min 3))
  (yafolding-mode +1)
  (setq buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table
                          'selective-display
                          (string-to-vector " ..."))

  (def-keys-for-map vim-normal-mode-local-keymap
    ("z" hydra-vim-normal-z-hideshow-yafolding-and-outline/body))
  (def-keys-for-map vim-visual-mode-local-keymap
    ("z" hydra-vim-visual-z-yafolding/body)))

(provide 'haskell-outline)

;; Local Variables:
;; End:

;; haskell-outline.el ends here
