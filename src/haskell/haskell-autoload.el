;; haskell-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 July 2012
;; Description:

(require 'mmm-setup)

(autoload 'ghc-core-create-core "ghc-core" nil t)
(autoload 'ghc-core-mode "ghc-core" nil t)
(autoload 'ghci-script-mode "ghci-script-mode" nil t)

(autoload 'haskell-cabal-mode "haskell-cabal" nil t)
(autoload 'haskell-cabal-guess-setting "haskell-cabal" nil t)
(autoload 'haskell-cabal-get-dir "haskell-cabal")
(autoload 'haskell-cabal-visit-file "haskell-cabal" nil t)

(autoload 'haskell-compile "haskell-compile" nil t)

(autoload 'haskell-font-lock-choose-keywords "haskell-font-lock")
(autoload 'haskell-mode "haskell-mode" nil t)
(autoload 'literate-haskell-mode "haskell-mode" nil t)
(autoload 'haskell-doc-mode "haskell-doc" nil t)
(autoload 'turn-on-haskell-doc-mode "haskell-doc" nil t)
(autoload 'haskell-doc-current-info "haskell-doc")
(autoload 'haskell-doc-show-type "haskell-doc" nil t)

(autoload 'haskell-navigate-imports "haskell-navigate-imports" nil t)
(autoload 'haskell-navigate-imports-return "haskell-navigate-imports" nil t)

(autoload 'turn-on-haskell-unicode-input-method "haskell-unicode-input-method" nil t)
(autoload 'haskell-mode-after-save-handler "haskell")
(autoload 'haskell-process-load-file "haskell" nil t)
(autoload 'switch-to-haskell "inf-haskell" nil t)

(autoload 'ghc-profiling-mode "ghc-profiling-mode" nil t)
(autoload 'ghc-profiling-mode-setup "ghc-profiling-mode")
(add-hook 'ghc-profiling-mode-hook #'ghc-profiling-mode-setup)
(add-to-list 'auto-mode-alist '("\\.prof\\'" . ghc-profiling-mode))

(autoload 'vim:ghc-core-create-core "haskell-setup" nil t)
(autoload 'vim:haskell-compile "haskell-setup" nil t)
(autoload 'vim:haskell-compile-choosing-command "haskell-setup" nil t)
(autoload 'vim:haskell-load-file-into-repl "haskell-setup" nil t)
(autoload 'vim:haskell-set-target "haskell-setup" nil t)
(autoload 'vim:haskell-interactive-clear-buffer-above-prompt "haskell-setup" nil t)
(autoload 'vim:haskell-flycheck-run "haskell-setup" nil t)
(autoload 'vim:haskell-flycheck-compile "haskell-setup" nil t)
(autoload 'vim:haskell-flycheck-configure "haskell-setup" nil t)
(autoload 'vim:haskell-flycheck-clear "haskell-setup" nil t)
(autoload 'vim:haskell-flycheck-list-errors "haskell-setup" nil t)
(autoload 'vim:haskell-navigate-imports "haskell-setup" nil t)

(autoload 'intero-mode "intero" nil t)

(defparameter *haskell-extensions* '("hs" "lhs" "hsc" "chs" "hs-boot" "lhs-boot" "alex" "x" "happy" "y"))

(autoload 'haskell-setup "haskell-setup")
(autoload 'inferior-haskell-mode-setup "haskell-setup")
(autoload 'haskell-c2hs-mode "haskell-c2hs" nil t)
(autoload 'ghc-check-mode "ghc-check-mode")

(autoload 'haskell-setup "haskell-setup")
(autoload 'haskell-compilation-setup "haskell-setup")
(autoload 'ghc-check-mode-setup "haskell-setup")
(autoload 'haskell-cabal-setup "haskell-setup")
(autoload 'ghc-core-setup "haskell-setup")

(mmm-add-classes
 '((literate-haskell-latex
    :submode haskell-mode
    ;; :face font-lock-function-name-face ;; mmm-output-submode-face
    :front "^\\\\begin{\\(?:\\(?:new\\)?code\\|spec\\)}$"
    :include-front nil
    :front-offset (end-of-line 1)
    :back "^\\\\end{\\(?:\\(?:new\\)?code\\|spec\\)}$"
    :include-back nil
    :back-offset (beginning-of-line -1)

    ;; :front-verify haskell-blocks-verify-front
    ;; :back haskell-blocks-find-back
    ;; :back-verify haskell-blocks-verify-back
    )))

(add-to-list 'auto-mode-alist '("\\.lhs\\'" . LaTeX-mode))
(mmm-add-mode-ext-class 'LaTeX-mode "\\.lhs\\'" 'literate-haskell-latex)
(mmm-add-mode-ext-class 'latex-mode "\\.lhs\\'" 'literate-haskell-latex)

(add-to-list 'auto-mode-alist '("\\.ghci\\'" . ghci-script-mode))
(add-to-list 'auto-mode-alist '("\\.hcr\\'" . ghc-core-mode))
(add-to-list 'auto-mode-alist '("cabal\\.config.*\\'" . haskell-cabal-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\(?:[./\\]config.*\\)?\\'" . haskell-cabal-mode))
(add-to-list 'auto-mode-alist '("\\.hs\\(?:-boot\\)?\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs\\(?:-boot\\)?\\'" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.chs\\'" . haskell-c2hs-mode))

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(defalias 'ghci 'switch-to-haskell)

(put 'shm-display-quarantine 'safe-local-variable #'booleanp)

(add-hook 'haskell-mode-hook #'haskell-setup)
(add-hook 'literate-haskell-mode-hook #'haskell-setup)
(add-hook 'inferior-haskell-mode-hook #'inferior-haskell-mode-setup)
(add-hook 'haskell-interactive-mode-hook #'haskell-interactive-mode-setup)
(add-hook 'haskell-compilation-mode-hook #'haskell-compilation-setup)
(add-hook 'ghc-check-mode-hook #'ghc-check-mode-setup)
(add-hook 'haskell-cabal-mode-hook #'haskell-cabal-setup)
(add-hook 'ghc-core-mode-hook #'ghc-core-setup)

;; grammar tools autoloads

(require 'happy-mode-autoload)

(autoload 'alex-mode "alex-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.alex\\'" . alex-mode))
(add-to-list 'auto-mode-alist '("\\.x\\'" . alex-mode))
(mmm-add-mode-ext-class 'alex-mode "\\.x\\'" 'haskell-blocks)

(add-to-list 'auto-mode-alist '("\\.happy\\'" . happy-mode))
(add-to-list 'auto-mode-alist '("\\.ly\\'" . happy-mode))
(add-to-list 'auto-mode-alist '("\\.y\\'" . happy-mode))
(mmm-add-mode-ext-class 'happy-mode "\\.y\\'" 'haskell-blocks)

(provide 'haskell-autoload)

;; Local Variables:
;; End:

;; haskell-autoload.el ends here
