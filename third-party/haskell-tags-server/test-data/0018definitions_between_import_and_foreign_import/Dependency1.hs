----------------------------------------------------------------------------
-- |
-- Module      :  Dependency1
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Dependency1
  ( module Dependency1
  , module Dependency2
  ) where

foo :: Int -> Int
foo = id

import Dependency2

bar :: Int -> Int
bar = id

foreign import ccall unsafe "test" test :: Int -> Int

baz :: Int -> Int
baz = id
