;; haskell-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 26 November 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'ert)

(ert-deftest haskell-tests/toplevel-signature-regexp ()
  "Test that `haskell-toplevel-signature-regexp' really occuring signatures."
  (should (string-match-pure? haskell-toplevel-signature-regexp
                              "\
rootPath :: FilePath -> ModuleName l -> FilePath"))
  (should (string-match-pure? haskell-toplevel-signature-regexp
                              "\
modNameToPath
  :: FilePath -- ^ root path
  -> ModuleNameS -- ^ module name
  -> FilePath -- ^ module path"))
  )

(ert-deftest haskell-tests/haskell-peg-parse-string ()
  (should (haskell-peg-parse-string
           (type-name)
           "Int"))
  (should (haskell-peg-parse-string
           (type-name)
           "Int#"))
  (should (haskell-peg-parse-string
           (type-name)
           "Double#"))

  (should (haskell-peg-parse-string
           (type-name)
           "Map Int Int"))
  (should (haskell-peg-parse-string
           (type-name)
           "Map Int Super_Int'"))
  (should (haskell-peg-parse-string
           (type-name)
           "Map.Map Int Int"))
  (should (haskell-peg-parse-string
           (type-name)
           "Map.Map Int T.Text"))

  (should (haskell-peg-parse-string
           (type-name)
           "(a, b)"))
  (should (haskell-peg-parse-string
           (type-name)
           "(# a, b #)"))

  (should (haskell-peg-parse-string
           (type-name)
           "(# a, Int# #)"))

  (should (haskell-peg-parse-string
           (type-name)
           "IO ()"))

  (should (haskell-peg-parse-string
           (type-name)
           "IO [ Maybe (foo, Bar Baz a,   Quux   Xuuq      )    ]"))
  (should (haskell-peg-parse-string
           (type-name newline)
           "IO [Maybe (foo, Bar Baz a, Fizz Buzz)]\nfibur"))

  (should (haskell-peg-parse-string
           (type-name)
           "(a -> b)"))
  (should (haskell-peg-parse-string
           (type-name)
           "(f a -> b)"))
  (should (haskell-peg-parse-string
           (type-name)
           "((a -> b) -> f a -> f b)"))

  (should (haskell-peg-parse-string
           (type-name)
           "a -> b"))
  (should (haskell-peg-parse-string
           (type-name)
           "(a -> (b, c -> d -> e))"))

  (should (haskell-peg-parse-string
           (type-name)
           "[Token] -> Maybe (Token, [Token])"))

  (should (haskell-peg-parse-string
           (func-name)
           "(>>=#)")))

(ert-deftest haskell-tests/haskell-parse-signature ()
  (should (haskell-parse-signature
           "extractInfixConstructor :: [Token] -> Maybe (Token, [Token])"))
  (should (haskell-parse-signature
           "extractInfixConstructor :: (Num a) => [a] -> Maybe (Token, [Token])"))
  (should (haskell-parse-signature
           "extractInfixConstructor :: (Num a, Num z) => [Foo a z] -> Maybe (Token, [Token])"))

  (should (haskell-parse-signature
           "f :: Int# -> Double#"))
  (should (haskell-parse-signature
           "f :: (# Int#, Int# #) -> Double#"))
  (should (haskell-parse-signature
           "f, g, h :: a -> b"))
  (should (haskell-parse-signature
           "(+) :: a -> b"))
  (should (haskell-parse-signature
           "(+), (*), (<*>) :: a -> b"))
  (should (haskell-parse-signature
           "(+), (*), (<*>), f, g, h :: a -> b"))

  (should (haskell-parse-signature
           "(+), (*), (<*>), f, g, h :: (Real a, Show b) => a -> b"))

  (should (haskell-parse-signature
           "(>>=#) :: (Monad m) => m b -> (b -> m c) -> m c"))
  (should (haskell-parse-signature
           "(>>=#) :: (M.Monad m) => m b -> (b -> m c) -> m c"))
  (should (haskell-parse-signature
           "search :: Map.Map k v -> k -> v"))
  (should (haskell-parse-signature
           "search :: (Ord k) => Map.Map k v -> k -> v")))


(setf haskell-tests/tests
      '(haskell-tests/toplevel-signature-regexp
        haskell-tests/haskell-peg-parse-string
        haskell-tests/haskell-parse-signature))

(let ((ert-debug-on-error nil))
  (eproj-reset-projects)
  (ert (join-lines (map #'symbol->string haskell-tests/tests) "\\|")
       ;; "haskell-tests/.*"
       )
  nil)



;; Local Variables:
;; no-byte-compile: t
;; End:

;; haskell-tests.el ends here
