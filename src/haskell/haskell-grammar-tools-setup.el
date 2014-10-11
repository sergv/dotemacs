;; haskell-grammar-tools-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 13 May 2014
;; Description:

(require 'common)
(require 'mmm-auto)
(require 'happy-mode-autoload)

(setf mmm-global-mode 'maybe
      mmm-submode-decoration-level 0)

(add-to-list 'auto-mode-alist '("\\.alex\\'" . alex-mode))
(add-to-list 'auto-mode-alist '("\\.x\\'" . alex-mode))
(mmm-add-mode-ext-class 'alex-mode "\\.x\\'" 'haskell-blocks)

(add-to-list 'auto-mode-alist '("\\.happy\\'" . happy-mode))
(add-to-list 'auto-mode-alist '("\\.ly\\'" . happy-mode))
(add-to-list 'auto-mode-alist '("\\.y\\'" . happy-mode))
(mmm-add-mode-ext-class 'happy-mode "\\.y\\'" 'haskell-blocks)

(defun haskell-grammar-tools-setup ()
  (init-common :use-yasnippet nil :use-render-formula nil)
  (hs-minor-mode 1)
  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<f9>" haskell-compile)))

(add-hook 'alex-mode-hook #'haskell-grammar-tools-setup)
(add-hook 'happy-mode-hook #'haskell-grammar-tools-setup)

(provide 'haskell-grammar-tools-setup)

;; Local Variables:
;; End:

;; haskell-grammar-tools-setup.el ends here
