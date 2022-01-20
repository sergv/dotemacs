;; haskell-grammar-tools-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 13 May 2014
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common)
(require 'haskell-misc)
(require 'haskell-outline)
(require 'haskell-setup)
(require 'hydra-setup)

(require 'mmm-setup)

(defhydra-derive hydra-haskell-grammar-tools-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign
_i_: jump to imports
_I_: jump back"
  ("a"     hydra-haskell-align/body)
  ("i"     vim:haskell-navigate-imports:interactive)
  ("I"     haskell-navigate-imports-return))

;;;###autoload
(defun haskell-grammar-tools-setup ()
  (init-common :use-whitespace 'tabs-only)
  (fontify-merge-markers)
  (let ((proj (eproj-get-project-for-buf-lax (current-buffer))))
    (haskell-setup-indentation
     :offset (eproj-query/haskell/indent-offset proj)
     :simpler-indentation-by-default t)
    (setq-local tab-always-indent t
                indent-line-function
                (lambda ()
                  (indent-to standard-indent))))
  (haskell-setup-folding :enable-hs-minor-mode t)

  (install-haskell-smart-operators!
      vim-insert-mode-local-keymap
    :bind-colon t
    :bind-hyphen t)
  (setup-eproj-symbnav)

  (dolist (cmd '("c" "compile"))
    (vim-local-emap cmd  #'vim:haskell-compile))
  (dolist (cmd '("cc" "ccompile"))
    (vim-local-emap cmd  #'vim:haskell-compile-choosing-command))
  (def-keys-for-map vim-normal-mode-local-keymap
    ("g" hydra-haskell-grammar-tools-vim-normal-g-ext/body))
  (def-keys-for-map vim-visual-mode-local-keymap
    ("g" hydra-haskell-vim-visual-g-ext/body))
  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("M-t"             haskell-compilation-prev-error-other-window)
    ("M-h"             haskell-compilation-next-error-other-window)
    ("C-SPC"           company-complete)

    ("DEL"             haskell-backspace-with-block-dedent)
    ("<backspace>"     haskell-backspace-with-block-dedent)

    ("S-<tab>"         nil)
    ("<S-iso-lefttab>" nil)
    ("<return>"        haskell-newline-with-signature-expansion)
    (("C-m" "<f9>")    haskell-compile)))

(provide 'haskell-grammar-tools-setup)

;; Local Variables:
;; End:

;; haskell-grammar-tools-setup.el ends here
