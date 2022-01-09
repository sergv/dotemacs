----------------------------------------------------------------------------
-- |
-- Module      :  Foo
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Foo (foobar) where

import Subdep1.Foo

foobar :: Int -> Int
foobar = subfoo . (+42)
