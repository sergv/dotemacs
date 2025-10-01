;; haskell-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 July 2012
;; Description:

(require 'haskell-constants)

(add-hook 'ghc-profiling-mode-hook #'ghc-profiling-mode-setup)
(add-to-list 'auto-mode-alist '("\\.prof\\'" . ghc-profiling-mode))
;; Override regular ".prof" regexp for typical cabal.project with profiling options.
(add-to-list 'auto-mode-alist '("cabal\\.project\\.prof\\'" . haskell-cabal-project-mode))

(defun enable-hlint-after-dante-for-flycheck ()
  (flycheck-add-next-checker 'haskell-dante
                             '(warning . haskell-hlint)))

(add-hook 'dante-mode-hook #'enable-hlint-after-dante-for-flycheck)

;; (mmm-add-classes
;;  '((literate-haskell-latex
;;     :submode haskell-mode
;;     ;; :face font-lock-function-name-face ;; mmm-output-submode-face
;;     :front "^\\\\begin{\\(?:\\(?:new\\)?code\\|spec\\)}$"
;;     :include-front nil
;;     :front-offset (end-of-line 1)
;;     :back "^\\\\end{\\(?:\\(?:new\\)?code\\|spec\\)}$"
;;     :include-back nil
;;     :back-offset (beginning-of-line -1)
;;
;;     ;; :front-verify haskell-blocks-verify-front
;;     ;; :back haskell-blocks-find-back
;;     ;; :back-verify haskell-blocks-verify-back
;;     )))
;;
;; ;; (add-to-list 'auto-mode-alist '("\\.lhs\\(?:-boot\\)?\\'" . latex-mode))
;;
;; (dolist (mode '(LaTeX-mode latex-mode))
;;   (mmm-add-mode-ext-class mode "\\.lhs\\(?:-boot\\)\\'" 'literate-haskell-latex))

(add-to-list 'auto-mode-alist (cons (rx "."
                                        (or "hcr"
                                            "dump-simpl"
                                            "dump-splices"
                                            "dump-spec"
                                            "dump-str-signatures"
                                            "dump-stranal")
                                        eos)
                                    #'ghc-core-mode))

(add-to-list 'auto-mode-alist '("\\.ghci\\'" . ghci-script-mode))
(add-to-list 'auto-mode-alist '("\\.hs\\(?:-boot\\|ig\\)?\\'" . haskell-mode))

(add-to-list 'auto-mode-alist '("\\.lhs\\(?:-boot\\)?\\'" . haskell-literate-mode))

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(defalias 'ghci 'switch-to-haskell)

(put 'haskell-compile-cabal-build-command 'safe-local-variable #'stringp)
(put 'haskell-compile-command 'safe-local-variable #'stringp)
(put 'haskell-program-name 'safe-local-variable (lambda (x) (or (stringp x) (listp x))))
(put 'hindent-style 'safe-local-variable #'stringp)

(add-hook 'ghc-core-mode-hook #'ghc-core-mode-setup)
(add-hook 'haskell-cabal-mode-hook #'haskell-cabal-setup)
(add-hook 'haskell-compilation-mode-hook #'haskell-compilation-setup)
(add-hook 'dante-repl-mode-hook #'dante-repl-mode-setup)
(add-hook 'cmm-mode-hook #'cmm-setup)

(add-hook 'haskell-mode-hook #'haskell-setup-common-prelude)
(add-hook 'haskell-mode-hook #'haskell-setup t)

(add-hook 'haskell-ts-base-mode-hook #'haskell-setup-common-prelude)

(add-hook 'haskell-ts-mode-hook #'haskell-setup t)
(add-hook 'haskell-ts-mode-hook #'haskell-ts-setup t)
;; (add-hook 'haskell-literate-mode-hook #'haskell-setup)

(add-hook 'haskell-c2hs-mode-hook #'haskell-c2hs-setup)

(add-hook 'haskell-hsc-mode-hook #'haskell-hsc-setup t)
(add-hook 'haskell-hsc-mode-hook #'haskell-ts-setup t)


(add-hook 'alex-mode-hook #'haskell-grammar-tools-setup)
(add-hook 'happy-mode-hook #'haskell-grammar-tools-setup)
(add-hook 'uuag-mode-hook #'haskell-grammar-tools-setup)

(add-to-list 'auto-mode-alist '("\\.alex\\'" . alex-mode))
(add-to-list 'auto-mode-alist '("\\.x\\'" . alex-mode))

(add-to-list 'auto-mode-alist '("\\.happy\\'" . happy-mode))
(add-to-list 'auto-mode-alist '("\\.ly\\'" . happy-mode))
(add-to-list 'auto-mode-alist '("\\.y\\'" . happy-mode))

(provide 'haskell-autoload)

;; Local Variables:
;; End:

;; haskell-autoload.el ends here
