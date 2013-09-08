;; haskell-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 July 2012
;; Description:

(load "haskell-autoloads-generated")

(autoload 'haskell-setup "haskell-setup" "" nil nil)
(autoload 'inferior-haskell-mode-setup "haskell-setup" "" nil nil)

(add-to-list 'auto-mode-alist        '("\\.hcr\\'" . ghc-core-mode))
(add-to-list 'auto-mode-alist        '("\\.cabal\\'" . haskell-cabal-mode))
(add-to-list 'auto-mode-alist        '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist        '("\\.l[gh]s\\'" . literate-haskell-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))
(add-to-list 'auto-mode-alist        '("\\.hsc\\'" . haskell-c-mode))
(add-to-list 'auto-mode-alist        '("\\.chs\\'" . haskell-c-mode))

(defalias 'ghci 'switch-to-haskell)



(add-hook 'haskell-mode-hook #'haskell-setup)
(add-hook 'literate-haskell-mode-hook #'haskell-setup)

(add-hook 'inferior-haskell-mode-hook #'inferior-haskell-mode-setup)


;; Local Variables:
;; End:

;; haskell-autoload.el ends here
