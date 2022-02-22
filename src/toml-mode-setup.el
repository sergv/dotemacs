;; toml-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 28 July 2020
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common)
(require 'git-setup)

;;;###autoload
(defun toml-mode-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-whitespace t
               :use-fci t
               :use-hl-line t)

  (setq-local whitespace-line-column 80
              whitespace-style '(face tabs lines-tail))

  (def-keys-for-map (vim-normal-mode-local-keymap vim-insert-mode-local-keymap)
    ("<tab>"                               tab-to-tab-stop)
    (("<backtab>" "S-<tab>" "S-<iso-tab>") tab-to-tab-stop-backward))

  (def-keys-for-map vim-visual-mode-local-keymap
    ("g" hydra-gitconfig-vim-visual-g-ext/body))

  (setup-folding t '(:header-symbol "#" :length-min 3)))

;;;###autoload
(add-hook 'toml-mode-hook #'toml-mode-setup)

(provide 'toml-mode-setup)

;; Local Variables:
;; End:

;; toml-mode-setup.el ends here
