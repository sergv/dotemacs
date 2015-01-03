;; haskell-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 July 2012
;; Description:

(load-library "haskell-mode-autoloads")

(require 'ghc-prof-mode)
(remove-hook 'haskell-mode-hook 'ghc-prof-highlight-current)

(defparameter *haskell-extensions* '("hs" "lhs" "hsc" "chs" "hs-boot" "lhs-boot"))

(autoload 'haskell-setup "haskell-setup" "" nil nil)
(autoload 'inferior-haskell-mode-setup "haskell-setup" "" nil nil)
(autoload 'c2hs-mode "c2hs-mode" "" nil nil)

(add-to-list 'auto-mode-alist        '("\\.hcr\\'" . ghc-core-mode))
(add-to-list 'auto-mode-alist        '("\\.cabal\\'" . haskell-cabal-mode))
(add-to-list 'auto-mode-alist        '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist        '("\\.l[gh]s\\'" . literate-haskell-mode))
(add-to-list 'auto-mode-alist        '("\\.hs-boot\\'" . haskell-mode))
(add-to-list 'auto-mode-alist        '("\\.lhs-boot\\'" . literate-haskell-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))
(add-to-list 'auto-mode-alist        '("\\.hsc\\'" . haskell-c-mode))
(add-to-list 'auto-mode-alist        '("\\.chs\\'" . c2hs-mode))

(defalias 'ghci 'switch-to-haskell)



(add-hook 'haskell-mode-hook #'haskell-setup)
(add-hook 'literate-haskell-mode-hook #'haskell-setup)

(add-hook 'inferior-haskell-mode-hook #'inferior-haskell-mode-setup)

(provide 'haskell-autoload)

;; Local Variables:
;; End:

;; haskell-autoload.el ends here
