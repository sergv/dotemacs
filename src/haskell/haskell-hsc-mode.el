;; haskell-hsc-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 May 2018
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'haskell-mode)
(require 'haskell-font-lock)
(require 'haskell-utils)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-hsc-mode))

;;;###autoload
(define-derived-mode haskell-hsc-mode haskell-ts-base-mode "HSC"
  "Mode for editing *.hsc files of the hsc2hs Haskell tool.")

(provide 'haskell-hsc-mode)

;; Local Variables:
;; End:

;; haskell-hsc-mode.el ends here
