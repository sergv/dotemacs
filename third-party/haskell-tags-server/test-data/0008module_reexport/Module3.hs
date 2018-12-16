----------------------------------------------------------------------------
-- |
-- Module      :  Module3
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday,  2 November 2016
----------------------------------------------------------------------------

module Module3 (module Module3) where



foo3 :: a -> b -> a
foo3 x _ =
  x



bar3 :: Double -> Double -> Double
bar3 x y =
  x - y
