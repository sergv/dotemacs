;; haskell-snippets.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 11 May 2022
;; Description:

(require 'yasnippet-setup)

(defconst haskell-snippets--cabal-run-header--body
  "#!/usr/bin/env -S cabal run
{- cabal:
build-depends:
  , base
  , containers
  , filepath
  , directory
  , text${1:
  , ${2:vector ^>= 0.12}
default-language:
  GHC2024
ghc-options:
  -threaded -rtsopts \"-with-rtsopts=-A32m -s\"}
-}$(3:
{- project:
allow-newer:
  , *:base
-})
")

(defconst haskell-snippets--cabal-run-header-snippet
  (concat
   "# key: #!
# name: cabal run header for standalone scripts
# --\n"
   haskell-snippets--cabal-run-header--body))

(defun haskell-snippets-install! (mode)
  (let ((snippets (list haskell-snippets--cabal-run-header-snippet))
        (defs nil))
    (with-temp-buffer
      (dolist (snip snippets)
        (erase-buffer)
        (insert snip)
        (setf defs
              (append (yas--parse-multiple-templates)
                      defs))))
    (yas-define-snippets mode defs)))

(provide 'haskell-snippets)

;; Local Variables:
;; End:

;; haskell-snippets.el ends here
