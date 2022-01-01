;; lua-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 23 September 2012
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'hydra-setup)

;;;###autoload
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(defhydra-ext hydra-lua-align (:exit t :foreign-keys nil :hint nil)
  "
_a_: general
_=_: on equals"
  ("a" align)
  ("=" c-align-on-equals))

(defhydra-derive hydra-lua-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign"
  ("a" hydra-lua-align/body))

;;;###autoload
(defun lua-setup ()
  (init-common :use-yasnippet nil
               :use-render-formula t
               :sp-slurp-sexp-insert-space nil
               :use-whitespace 'tabs-only
               :use-fci t)
  (setup-folding t nil)
  (setup-indent-size 2)

  (def-keys-for-map vim:visual-mode-local-keymap
    ("g" hydra-lua-vim-visual-g-ext/body)))

;;;###autoload
(add-hook 'lua-mode-hook #'lua-setup)

(provide 'lua-setup)

;; Local Variables:
;; End:

;; lua-setup.el ends here
