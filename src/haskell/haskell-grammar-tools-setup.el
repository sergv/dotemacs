;; haskell-grammar-tools-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 13 May 2014
;; Description:

(require 'common)
(require 'haskell-misc)
(require 'haskell-outline)

(require 'mmm-setup)

;;;###autoload
(defun haskell-grammar-tools-setup ()
  (init-common :use-yasnippet t
               :use-render-formula nil
               :use-fci t
               :use-whitespace 'tabs-only)
  (fontify-merge-markers)
  (vim:local-emap "compile"  #'vim:haskell-compile)
  (vim:local-emap "c"        #'vim:haskell-compile)
  (vim:local-emap "ccompile" #'vim:haskell-compile-choosing-command)
  (vim:local-emap "cc"       #'vim:haskell-compile-choosing-command)
  (setq-local tab-always-indent t)
  (setq-local indent-line-function
              (lambda ()
                (indent-to standard-indent)))
  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<f9>" haskell-compile))
  (install-haskell-smart-operators!
      vim:insert-mode-local-keymap
    :bind-colon t
    :bind-hyphen t
    :use-shm nil)
  (setup-eproj-symbnav)
  (haskell-define-align-bindings! vim:visual-mode-local-keymap)
  (let ((proj (ignore-errors
                (eproj-get-project-for-buf (current-buffer)))))
    (haskell-setup-indentation
     (eproj-query/haskell/indent-offset proj)))
  (haskell-setup-folding :enable-hs-minor-mode t))

(provide 'haskell-grammar-tools-setup)

;; Local Variables:
;; End:

;; haskell-grammar-tools-setup.el ends here
