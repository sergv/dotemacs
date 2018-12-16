----------------------------------------------------------------------------
-- |
-- Module      :  Dependency
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Dependency where

foo :: Int -> Int
foo = id

data Foo = Foo
  { bar :: Int
  , baz :: Double
  }

