----------------------------------------------------------------------------
-- |
-- Module      :  Dependency
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Dependency (frob, ambiguous) where

import Dependency (bar)



ambiguous :: a -> a
ambiguous x = x

frob :: Int -> Int
frob = bar . bar . bar
