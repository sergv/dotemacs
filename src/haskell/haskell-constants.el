;; haskell-constants.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 25 January 2022
;; Description:

(defconst +haskell-extensions+
  '("hs" "hsig" "lhs" "hsc" "chs" "hs-boot" "lhs-boot" "alex" "x" "lx" "happy" "y" "ly" "ag" "lag"))

(defconst +haskell-syntax-modes+
  '(haskell-mode haskell-ts-mode haskell-literate-mode haskell-c-mode haskell-c2hs-mode alex-mode happy-mode uuag-mode)
  "List of modes that use haskell syntax.")

(defconst +haskell-default-checker+ 'haskell-dante ;;'lsp
  "Default flycheck checker to use if not configured.")

(provide 'haskell-constants)

;; Local Variables:
;; End:

;; haskell-constants.el ends here
