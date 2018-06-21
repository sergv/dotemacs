;; nix-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 21 June 2018
;; Description:

(require 'indentation)

(require 'nix-company)
(require 'nix-shebang)

;;;###autoload
(global-nix-prettify-mode +1)

;;;###autoload
(defun nix-setup ()
  (init-common :use-comment t :use-fci t :use-whitespace t)
  (fontify-conflict-markers!)

  (company-mode +1)
  (setq-local company-backends '(company-nix))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("C-SPC" company-complete)))

;;;###autoload
(add-hook 'nix-mode-hook #'nix-setup)

(provide 'nix-setup)

;; Local Variables:
;; End:

;; nix-setup.el ends here
