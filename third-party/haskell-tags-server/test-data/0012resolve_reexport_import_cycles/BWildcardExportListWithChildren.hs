----------------------------------------------------------------------------
-- |
-- Module      :  BWildcardExportListWithChildren
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module BWildcardExportListWithChildren
  ( FooB(..)
  , BarB(..)
  , pattern BazBP
  , quuxB
  , pattern FrobBP
  , QuuxB(..)
  , pattern QuuxBP
  , commonFunc
  , derivedB
  ) where

data FooB = FooB1
  { fooB1 :: Int
  , fooB2 :: !Double
  }

newtype BarB =
  BarB1
    { unBarB :: [Double] }

pattern BazBP :: Double -> Double -> BarB
pattern BazBP x y = BarB1 [x, y]

quuxB :: Int -> Int
quuxB x = x

pattern FrobBP :: Int -> FooB
pattern FrobBP x = FooB1 { fooB1 = x, fooB2 = 0 }

data QuuxB =
    QuuxB1 Int
  | QuuxB2

pattern QuuxBP :: Int -> QuuxB
pattern QuuxBP n = QuuxB1 n

commonFunc :: Double -> Double
commonFunc x = x + x * x

$([d|
  derivedB :: Int -> Int
  derivedB x = x
  |])
