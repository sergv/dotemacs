;; haskell-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 July 2012
;; Description:

(require 'happy-mode-autoload)
(require 'mmm-setup)

(eval-after-load "attrap"
  '(progn
     (setf attrap-flycheck-checkers-alist
           (append
            '((haskell-ghc . attrap-ghc-fixer)
              (haskell-stack-ghc . attrap-ghc-fixer))
            attrap-flycheck-checkers-alist))))

(add-hook 'ghc-profiling-mode-hook #'ghc-profiling-mode-setup)
(add-to-list 'auto-mode-alist '("\\.prof\\'" . ghc-profiling-mode))

(defun enable-hlint-after-dante-for-flycheck ()
  (flycheck-add-next-checker 'haskell-dante
                             '(warning . haskell-hlint)))

(add-hook 'dante-mode-hook #'enable-hlint-after-dante-for-flycheck)


(autoload 'vim:ghc-core-create-core "haskell-setup" nil t)
(autoload 'vim:haskell-compile "haskell-setup" nil t)
(autoload 'vim:haskell-compile-choosing-command "haskell-setup" nil t)
(autoload 'vim:haskell-load-file-into-repl "haskell-setup" nil t)
(autoload 'vim:haskell-set-target "haskell-setup" nil t)
(autoload 'vim:haskell-flycheck-run "haskell-setup" nil t)
(autoload 'vim:haskell-flycheck-compile "haskell-setup" nil t)
(autoload 'vim:haskell-flycheck-configure "haskell-setup" nil t)
(autoload 'vim:haskell-flycheck-clear "haskell-setup" nil t)
(autoload 'vim:haskell-flycheck-list-errors "haskell-setup" nil t)
(autoload 'vim:haskell-navigate-imports "haskell-setup" nil t)

(defconst +haskell-extensions+
  '("hs" "hsig" "lhs" "hsc" "chs" "hs-boot" "lhs-boot" "alex" "x" "lx" "happy" "y" "ly" "ag" "lag"))

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

(add-to-list 'auto-mode-alist '("\\.ghci\\'" . ghci-script-mode))
(add-to-list 'auto-mode-alist '("\\.\\(?:hcr\\|dump-\\(?:simpl\\|splices\\)\\)\\'" . ghc-core-mode))
(add-to-list 'auto-mode-alist '("cabal\\.\\(?:config\\|project\\).*\\'" . haskell-cabal-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\(?:[./\\]config.*\\)?\\'" . haskell-cabal-mode))
(add-to-list 'auto-mode-alist '("\\.hs\\(?:-boot\\|ig\\|c\\)?\\'" . haskell-mode))

(add-to-list 'auto-mode-alist '("\\.lhs\\(?:-boot\\)?\\'" . literate-haskell-mode))

;; (add-to-list 'auto-mode-alist '("\\.lhs\\(?:-boot\\)?\\'" . latex-mode))

(dolist (mode '(LaTeX-mode latex-mode))
  (mmm-add-mode-ext-class mode "\\.lhs\\(?:-boot\\)\\'" 'literate-haskell-latex))

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(defalias 'ghci 'switch-to-haskell)

(put 'haskell-compile-cabal-build-command 'safe-local-variable #'stringp)
(put 'haskell-compile-command 'safe-local-variable #'stringp)
(put 'haskell-program-name 'safe-local-variable (lambda (x) (or (stringp x) (listp x))))
(put 'hindent-style 'safe-local-variable #'stringp)

(add-hook 'ghc-core-mode-hook #'ghc-core-setup)
(add-hook 'haskell-cabal-mode-hook #'haskell-cabal-setup)
(add-hook 'haskell-compilation-mode-hook #'haskell-compilation-setup)
(add-hook 'haskell-mode-hook #'haskell-setup)
(add-hook 'dante-repl-mode-hook #'dante-repl-mode-setup)
(add-hook 'literate-haskell-mode-hook #'haskell-setup)

(add-hook 'haskell-c2hs-mode #'haskell-c2hs-setup)
(add-hook 'haskell-hsc-mode #'haskell-hsc-setup)


(add-hook 'alex-mode-hook #'haskell-grammar-tools-setup)
(add-hook 'happy-mode-hook #'haskell-grammar-tools-setup)
(add-hook 'uuag-mode-hook #'haskell-grammar-tools-setup)

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
