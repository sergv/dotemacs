----------------------------------------------------------------------------
-- |
-- Module      :  Module2
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday,  2 November 2016
----------------------------------------------------------------------------

module Module2 (foo2, bar2) where


foo2 :: a -> a
foo2 x =
  x


bar2 :: Int -> Int -> Int
bar2 x y =
  x - y


baz2 :: Int -> Int -> Int
baz2 x y =
  x `mod` y
