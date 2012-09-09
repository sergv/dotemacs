;; haskell-autoload.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 July 2012
;; Description:

(load "haskell-site-file")
;; (load (concat +emacs-config-path+ "/src/haskell/haskell-mode/haskell-site-file.el"))

(autoload 'haskell-setup "haskell-setup" "" nil nil)
(autoload 'inferior-haskell-mode-setup "haskell-setup" "" nil nil)

(add-to-list 'auto-mode-alist '("\\.hcr\\'" . ghc-core-mode))
(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-c-mode))
(add-to-list 'auto-mode-alist '("\\.chs\\'" . haskell-c-mode))

(defalias 'ghci 'switch-to-haskell)



(add-hook 'haskell-mode-hook #'haskell-setup)
(add-hook 'literate-haskell-mode-hook #'haskell-setup)

(add-hook 'inferior-haskell-mode-hook #'inferior-haskell-mode-setup)


;; Local Variables:
;; lexical-binding: t
;; End:

;; haskell-autoload.el ends here
