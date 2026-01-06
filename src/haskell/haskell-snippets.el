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
default-language:
  GHC2024
build-depends:
  , base
  , containers
  , directory
  , directory-ospath-streaming
  , file-io
  , filepath >= 1.5
  , optparse-applicative
  , text
${1:  , ${2:vector >= 0.12}
}ghc-options:
  -Weverything
  -Wno-all-missed-specialisations
  -Wno-implicit-prelude
  -Wno-missed-specialisations
  -Wno-missing-import-lists
  -Wno-missing-local-signatures
  -Wno-safe
  -Wno-unsafe
  -Wno-missing-deriving-strategies
  -Wno-missing-safe-haskell-mode
  -Wno-missing-kind-signatures
  -Wno-missing-role-annotations
  -Wno-missing-poly-kind-signatures${2:
  -Wno-unused-imports
  -Wno-unused-packages
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
