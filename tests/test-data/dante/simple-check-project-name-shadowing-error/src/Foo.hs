-- |
-- Module:     Foo
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Foo (foo) where

foo :: a -> a
foo x = baz $ myreplicate 100 [x]
  where
    bar x = x

    baz y = bar x
      where
        x = case y of
          [[x]] -> x
          _ -> error "todo"

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = x : myreplicate (n - 1) x
