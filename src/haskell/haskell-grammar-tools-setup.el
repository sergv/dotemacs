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

;;;###autoload
(defun haskell-grammar-tools-setup ()
  (init-common :use-whitespace 'tabs-only)
  (let ((proj (eproj-get-project-for-buf-lax (current-buffer))))

    (haskell-compilation-commands-install! proj)

    (haskell-setup-indentation
     :offset (eproj-query/haskell/indent-offset proj)
     :simpler-indentation-by-default t)
    (setq-local tab-always-indent t
                indent-line-function
                (lambda ()
                  (indent-to standard-indent))))
  (haskell-setup-folding)

  (setq-local beginning-of-defun-function #'haskell-move-to-topmost-start-impl)

  (install-haskell-smart-operators!
      vim-insert-mode-local-keymap
    :bind-colon t
    :bind-hyphen t
    :track-extensions? t)
  (setup-eproj-symbnav)

  (dolist (cmd '("c" "compile"))
    (vim-local-emap cmd  #'vim:haskell-compile))
  (dolist (cmd '("cc" "ccompile"))
    (vim-local-emap cmd  #'vim:haskell-compile-choosing-command))
  (def-keys-for-map vim-normal-mode-local-keymap
    ("g" hydra-haskell-vim-normal-g-ext/body))
  (def-keys-for-map vim-visual-mode-local-keymap
    ("g" hydra-haskell-vim-visual-g-ext/body))

  (haskell-setup-common-editing)

  (def-keys-for-map (vim-normal-mode-local-keymap vim-insert-mode-local-keymap)
    ("M-t"             compilation-navigation-prev-error-other-window)
    ("M-h"             compilation-navigation-next-error-other-window)
    ("C-SPC"           company-complete)

    ("DEL"             haskell-backspace-with-block-dedent)
    ("<backspace>"     haskell-backspace-with-block-dedent)

    ("S-<tab>"         nil)
    ("<S-iso-lefttab>" nil)
    ("<return>"        haskell-newline-with-signature-expansion)))

(provide 'haskell-grammar-tools-setup)

;; Local Variables:
;; End:

;; haskell-grammar-tools-setup.el ends here
