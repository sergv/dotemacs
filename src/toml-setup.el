;; toml-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 22 March 2018
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common)
(require 'git-setup)

;;;###autoload
(defun toml-setup ()
  (init-common :use-render-formula nil
               :sp-slurp-sexp-insert-space nil
               :use-yasnippet t
               :use-whitespace 'tabs-only)

  (setq-local whitespace-line-column 80
              whitespace-style '(face tabs lines-tail))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-visual-mode-local-keymap)
    ("g" hydra-gitconfig-align/body)))

(add-hook 'toml-mode-hook #'toml-setup)

(provide 'toml-setup)

;; Local Variables:
;; End:

;; toml-setup.el ends here
