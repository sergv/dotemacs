;; rust-autoloads.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 22 February 2020
;; Description:

(defconst +rust-extensions+
  '("rs"))

(defconst +rust-modes+
  '(rust-ts-mode))

(add-hook 'rust-ts-mode-hook #'rust-ts-setup)

(provide 'rust-autoloads)

;; Local Variables:
;; End:

;; rust-autoloads.el ends here
