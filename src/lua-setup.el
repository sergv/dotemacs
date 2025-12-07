;; lua-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 23 September 2012
;; Description:

(eval-when-compile
  (require 'lua-mode)
  (require 'macro-util))

(require 'hydra-setup)

;;;###autoload
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(setf lua-indent-level 2)

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
               :use-whitespace 'tabs-only
               :use-fci t)
  (setq-local vim-bounds-of-string-guess-start
              #'vim--bounds-of-string--guess-via-enclosing-smaller-indent)
  (hs-minor-mode-initialize
   :start (eval-when-compile (concat
                              "\\(:?"
                              (regexp-opt (mapcar #'car lua-sexp-alist) 'words)
                              "\\)\\|[({]"))
   :end (eval-when-compile
          (concat
           "\\(:?"
           (regexp-opt (mapcar #'cdr lua-sexp-alist) 'words)
           "\\)\\|[)}]"))
   :forward-sexp
   #'lua-forward-sexp)
  (setup-folding t nil)
  (setup-indent-size 2)

  (def-keys-for-map vim-visual-mode-local-keymap
    ("g" hydra-lua-vim-visual-g-ext/body)))

;;;###autoload
(add-hook 'lua-mode-hook #'lua-setup)

(provide 'lua-setup)

;; Local Variables:
;; End:

;; lua-setup.el ends here
