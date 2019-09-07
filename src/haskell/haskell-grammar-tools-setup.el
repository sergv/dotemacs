;; haskell-grammar-tools-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 13 May 2014
;; Description:

(require 'common)
(require 'haskell-misc)
(require 'haskell-outline)
(require 'haskell-setup)
(require 'haskell-watch)

(require 'mmm-setup)

;;;###autoload
(defun haskell-grammar-tools-setup ()
  (init-common :use-whitespace 'tabs-only)
  (fontify-merge-markers)
  (haskell-watch-register-current-buffer!)
  (let ((proj (eproj-get-project-for-buf-lax (current-buffer))))
    (haskell-setup-indentation
     :offset (eproj-query/haskell/indent-offset proj)
     :simpler-indentation-by-default t)
    (setq-local tab-always-indent t)
    (setq-local indent-line-function
                (lambda ()
                  (indent-to standard-indent))))
  (haskell-setup-folding :enable-hs-minor-mode t)

  (install-haskell-smart-operators!
      vim:insert-mode-local-keymap
    :bind-colon t
    :bind-hyphen t)
  (setup-eproj-symbnav)
  (haskell-define-align-bindings! vim:visual-mode-local-keymap)

  (dolist (cmd '("c" "compile"))
    (vim:local-emap cmd  #'vim:haskell-compile))
  (dolist (cmd '("cc" "ccompile"))
    (vim:local-emap cmd  #'vim:haskell-compile-choosing-command))
  (def-keys-for-map vim:normal-mode-local-keymap
    ("g i" vim:haskell-navigate-imports)
    ("g I" haskell-navigate-imports-return))
  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("M-t"             haskell-compilation-prev-error-other-window)
    ("M-h"             haskell-compilation-next-error-other-window)
    ("C-SPC"           company-complete)
    ("C-b"             switch-to-buffer-or-file-in-current-project)
    ("M-b"             switch-to-buffer-or-file-in-current-or-related-projects)

    ("C-l"             intero-repl-load)

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
