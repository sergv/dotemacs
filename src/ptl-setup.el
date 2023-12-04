;; ptl-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  4 December 2023
;; Description:

;;;###autoload
(defun ptl-mode-setup ()
  (init-common :use-comment t
               :use-whitespace 'tabs-and-trailing-only)
  (setup-hideshow-yafolding t nil)

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-visual-mode-local-keymap
                     vim-motion-mode-local-keymap
                     vim-operator-pending-mode-local-keymap)
    ("'" vim:haskell-backward-up-indentation-or-sexp:interactive)
    ("q" vim:haskell-up-sexp:interactive)))

;;;###autoload
(add-hook 'ptl-mode-hook #'ptl-mode-setup)

(provide 'ptl-setup)

;; Local Variables:
;; End:

;; ptl-setup.el ends here
