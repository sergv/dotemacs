----------------------------------------------------------------------------
-- |
-- Module      :  Subdep1.Foo
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Subdep1.Foo where

import Subdep2.Bar

subfoo :: Int -> Int
subfoo = subbar . (+10)
