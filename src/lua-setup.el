;; lua-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 23 September 2012
;; Description:


(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


(defun lua-setup ()
  (init-common :use-yasnippet nil
               :use-render-formula t
               :sp-slurp-sexp-insert-space nil)
  (hs-minor-mode 1)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("z o" hs-show-block)
    ("z c" hs-hide-block)
    ("z C" hs-hide-all)
    ("z O" hs-show-all)))

(add-hook 'lua-mode-hook #'lua-setup)

(provide 'lua-setup)

;; Local Variables:
;; End:

;; lua-setup.el ends here
