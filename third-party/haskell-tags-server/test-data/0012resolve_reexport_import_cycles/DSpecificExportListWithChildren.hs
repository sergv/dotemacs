----------------------------------------------------------------------------
-- |
-- Module      :  DSpecificExportListWithChildren
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module DSpecificExportListWithChildren
  ( FooD(FooD1)
  , BarD(BarD1, unBarD)
  , pattern BazDP
  , quuxD
  , pattern FrobDP
  , QuuxD(QuuxD2)
  , pattern QuuxDP
  , commonFunc
  , derivedD
  ) where


data FooD = FooD1
  { fooD1 :: Int
  , fooD2 :: !Double
  }

newtype BarD =
  BarD1
    { unBarD :: [Double] }

pattern BazDP :: Double -> Double -> BarD
pattern BazDP x y = BarD1 [x, y]

quuxD :: Int -> Int
quuxD x = x

pattern FrobDP :: Int -> FooD
pattern FrobDP x = FooD1 { fooD1 = x, fooD2 = 0 }

data QuuxD =
    QuuxD1 Int
  | QuuxD2

pattern QuuxDP :: Int -> QuuxD
pattern QuuxDP n = QuuxD1 n

commonFunc :: Double -> Double
commonFunc x = x + x * x

$([d|
  derivedD :: Int -> Int
  derivedD x = x
  |])
