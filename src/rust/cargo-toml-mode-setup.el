;; cargo-toml-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 28 July 2020
;; Description:

(require 'toml-mode)
(require 'rust-compilation-commands)

;;;###autoload
(defun cargo-toml-mode-setup ()
  (rust-compilation-commands-install!
   (eproj-get-project-for-buf-lax (current-buffer))))

;;;###autoload
(add-hook 'cargo-toml-mode-hook #'cargo-toml-mode-setup)

(provide 'cargo-toml-mode-setup)

;; Local Variables:
;; End:

;; cargo-toml-mode-setup.el ends here
