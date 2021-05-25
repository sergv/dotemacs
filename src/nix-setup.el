;; nix-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 21 June 2018
;; Description:

(require 'indentation)

(require 'nix-company)
(require 'nix-shebang)

(awhen (getenv "EMACS_NIX_STORE_DIR")
  (setf nix-store-dir it))

(awhen (getenv "EMACS_NIX_STATE_DIR")
  (setf nix-state-dir it))

;;;###autoload
(nix-prettify-global-mode +1)

;;;###autoload
(defun nix-setup ()
  (init-common :use-whitespace t)
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
