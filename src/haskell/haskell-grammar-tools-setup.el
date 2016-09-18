;; haskell-grammar-tools-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 13 May 2014
;; Description:

(require 'common)
(require 'mmm-auto)
(require 'haskell-misc)
(require 'haskell-outline)

(setf mmm-global-mode 'maybe
      mmm-submode-decoration-level 0)

(defun haskell-grammar-tools-setup ()
  (init-common :use-yasnippet t
               :use-render-formula nil
               :use-fci t
               :use-whitespace 'tabs-only)
  (fontify-merge-markers)
  (hs-minor-mode 1)
  (vim:local-emap "compile"  #'vim:haskell-compile)
  (vim:local-emap "c"        #'vim:haskell-compile)
  (vim:local-emap "ccompile" #'vim:haskell-compile-choosing-command)
  (vim:local-emap "cc"       #'vim:haskell-compile-choosing-command)
  (setq-local vim:shift-width 2)
  (setq-local standard-indent 2)
  (setq-local tab-always-indent t)
  (setq-local indent-line-function
              (lambda ()
                (indent-to standard-indent)))
  (bind-tab-keys #'haskell-shm-tab-or-indent-relative-forward
                 #'haskell-shm-backtab-or-indent-relative-backward
                 :enable-yasnippet t)
  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<f9>" haskell-compile))
  (haskell-define-align-bindings vim:visual-mode-local-keymap)
  (haskell-abbrev+-setup)
  (haskell-setup-folding))

(provide 'haskell-grammar-tools-setup)

;; Local Variables:
;; End:

;; haskell-grammar-tools-setup.el ends here
