;; cargo-toml-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 28 July 2020
;; Description:

(require 'toml-mode)

;;;###autoload
(define-derived-mode cargo-toml-mode toml-mode "Cargo TOML")

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (rx "Cargo.toml" eos)
                   'cargo-toml-mode))

(provide 'cargo-toml-mode)

;; Local Variables:
;; End:

;; cargo-toml-mode.el ends here
