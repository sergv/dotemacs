----------------------------------------------------------------------------
-- |
-- Module      :  Dependency
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Dependency (bar, ambiguous) where

bar :: Int -> Int
bar x = x + 1

ambiguous :: Double -> Double
ambiguous x = x
