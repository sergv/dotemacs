;; haskell-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 July 2012
;; Description:

(require 'mmm-setup)

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

(defconst +haskell-extensions+
  '("hs" "lhs" "hsc" "chs" "hs-boot" "lhs-boot" "alex" "x" "lx" "happy" "y" "ly"))

(provide 'haskell-autoload)

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
(add-to-list 'auto-mode-alist '("\\.\\(?:hcr\\|dump-\\(?:simpl\\|splices\\)\\)\\'" . ghc-core-mode))
(add-to-list 'auto-mode-alist '("cabal\\.\\(?:config\\|project\\).*\\'" . haskell-cabal-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\(?:[./\\]config.*\\)?\\'" . haskell-cabal-mode))
(add-to-list 'auto-mode-alist '("\\.hs\\(?:-boot\\)?\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs\\(?:-boot\\)?\\'" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.chs\\'" . haskell-c2hs-mode))

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(defalias 'ghci 'switch-to-haskell)

(put 'haskell-compile-cabal-build-command 'safe-local-variable #'stringp)
(put 'haskell-compile-command 'safe-local-variable #'stringp)
(put 'haskell-program-name 'safe-local-variable (lambda (x) (or (stringp x) (listp x))))
(put 'hindent-style 'safe-local-variable #'stringp)
(put 'intero-targets 'safe-local-variable (lambda (x) (and (listp x) (cl-every #'stringp x ))))
(put 'shm-display-quarantine 'safe-local-variable #'booleanp)

(add-hook 'ghc-core-mode-hook #'ghc-core-setup)
(add-hook 'haskell-cabal-mode-hook #'haskell-cabal-setup)
(add-hook 'haskell-compilation-mode-hook #'haskell-compilation-setup)
(add-hook 'haskell-interactive-mode-hook #'haskell-interactive-mode-setup)
(add-hook 'haskell-mode-hook #'haskell-setup)
(add-hook 'inferior-haskell-mode-hook #'inferior-haskell-mode-setup)
(add-hook 'intero-repl-mode-hook #'intero-repl-mode-setup)
(add-hook 'literate-haskell-mode-hook #'haskell-setup)

(add-hook 'alex-mode-hook #'haskell-grammar-tools-setup)
(add-hook 'happy-mode-hook #'haskell-grammar-tools-setup)

(require 'happy-mode-autoload)

(add-to-list 'auto-mode-alist '("\\.alex\\'" . alex-mode))
(add-to-list 'auto-mode-alist '("\\.x\\'" . alex-mode))
(mmm-add-mode-ext-class 'alex-mode "\\.x\\'" 'haskell-blocks)

(add-to-list 'auto-mode-alist '("\\.happy\\'" . happy-mode))
(add-to-list 'auto-mode-alist '("\\.ly\\'" . happy-mode))
(add-to-list 'auto-mode-alist '("\\.y\\'" . happy-mode))
(mmm-add-mode-ext-class 'happy-mode "\\.y\\'" 'haskell-blocks)

;; Local Variables:
;; End:

;; haskell-autoload.el ends here
