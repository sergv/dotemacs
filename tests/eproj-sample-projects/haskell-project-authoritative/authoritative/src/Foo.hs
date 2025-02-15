----------------------------------------------------------------------------
-- |
-- Module      :  Foo
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Foo where

foo :: Int -> Int
foo = id

bar :: Double
bar = 3.1415

data Foo = FooConstructor Int
