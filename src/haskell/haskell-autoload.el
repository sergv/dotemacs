;; haskell-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 July 2012
;; Description:

(autoload 'ghc-core-create-core "ghc-core" nil t)
(autoload 'ghc-core-mode "ghc-core" nil t)
(autoload 'ghci-script-mode "ghci-script-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ghci\\'" . ghci-script-mode))

(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(autoload 'haskell-cabal-mode "haskell-cabal" nil t)
(autoload 'haskell-cabal-guess-setting "haskell-cabal" nil t)
(autoload 'haskell-cabal-get-dir "haskell-cabal" nil nil)
(autoload 'haskell-cabal-visit-file "haskell-cabal" nil t)

(autoload 'haskell-compile "haskell-compile" nil t)

(autoload 'haskell-font-lock-choose-keywords "haskell-font-lock" nil nil)
(autoload 'haskell-mode "haskell-mode" nil t)
(autoload 'literate-haskell-mode "haskell-mode" nil t)

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))
(add-to-list 'completion-ignored-extensions ".hi")

(autoload 'turn-on-haskell-unicode-input-method "haskell-unicode-input-method" nil t)

(autoload 'ghc-prof-mode "ghc-prof-mode" nil t)
(eval-after-load "ghc-prof-mode"
  '(progn
     (remove-hook 'haskell-mode-hook #'ghc-prof-highlight-current)
     (add-hook 'ghc-prof-mode-hook #'ghc-prof-clear)))

(defparameter *haskell-extensions* '("hs" "lhs" "hsc" "chs" "hs-boot" "lhs-boot"))

(autoload 'haskell-setup "haskell-setup" "" nil nil)
(autoload 'inferior-haskell-mode-setup "haskell-setup" "" nil nil)
(autoload 'c2hs-mode "c2hs-mode" "" nil nil)
(autoload 'ghc-check-mode "ghc-check-mode" "" nil nil)

(autoload 'haskell-setup "haskell-setup" "" nil nil)
(autoload 'haskell-compilation-setup "haskell-setup" "" nil nil)
(autoload 'ghc-check-mode-setup "haskell-setup" "" nil nil)
(autoload 'haskell-cabal-setup "haskell-setup" "" nil nil)
(autoload 'hs-lint-setup "haskell-setup" "" nil nil)
(autoload 'ghc-core-setup "haskell-setup" "" nil nil)
(autoload 'hs-lint-setup "haskell-setup" "" nil nil)


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
(add-hook 'haskell-interactive-mode-hook #'haskell-interactive-mode-setup)
(add-hook 'haskell-compilation-mode-hook #'haskell-compilation-setup)
(add-hook 'ghc-check-mode-hook #'ghc-check-mode-setup)
(add-hook 'haskell-cabal-mode-hook #'haskell-cabal-setup)
(add-hook 'hs-lint-setup-hook #'hs-lint-setup)
(add-hook 'ghc-core-mode-hook #'ghc-core-setup)
(add-hook 'hs-lint-mode-hook #'hs-lint-setup)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(eval-after-load "ghc"
  '(progn
     ;; debug output will be in ghc-debug-buffer, "*GHC Debug*"
     (setf ghc-debug t
           ghc-ghc-options '("-isrc" "-dsuppress-module-prefixes")
           ghc-display-error 'other-buffer
           ghc-display-hole 'other-buffer)))

(provide 'haskell-autoload)

;; Local Variables:
;; End:

;; haskell-autoload.el ends here
