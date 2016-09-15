;; lua-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 23 September 2012
;; Description:

;;;###autoload
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;;###autoload
(defun lua-setup ()
  (init-common :use-yasnippet nil
               :use-render-formula t
               :sp-slurp-sexp-insert-space nil
               :use-whitespace 'tabs-only
               :use-fci t)
  (hs-minor-mode-setup))

;;;###autoload
(add-hook 'lua-mode-hook #'lua-setup)

(provide 'lua-setup)

;; Local Variables:
;; End:

;; lua-setup.el ends here
