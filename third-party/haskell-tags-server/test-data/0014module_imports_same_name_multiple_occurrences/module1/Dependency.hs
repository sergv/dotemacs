----------------------------------------------------------------------------
-- |
-- Module      :  Dependency
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Dependency (foo, foofoo, ambiguous) where

import Foo (bar, foofoo)

ambiguous :: a -> a
ambiguous x = x

foo :: Int -> Int
foo = bar . foofoo . bar
