----------------------------------------------------------------------------
-- |
-- Module      :  ESpecificExportListWithChildrenPlusSome
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module ESpecificExportListWithChildrenPlusSome
  ( FooE(FooE1, FrobEP)
  , BarE(BarE1, unBarE, BazEP)
  , quuxE
  , QuuxE(QuuxE2, QuuxEP)
  , commonFunc
  , derivedE
  ) where

data FooE = FooE1
  { fooE1 :: Int
  , fooE2 :: !Double
  }

newtype BarE =
  BarE1
    { unBarE :: [Double] }

pattern BazEP :: Double -> Double -> BarE
pattern BazEP x y = BarE1 [x, y]

quuxE :: Int -> Int
quuxE x = x

pattern FrobEP :: Int -> FooE
pattern FrobEP x = FooE1 { fooE1 = x, fooE2 = 0 }

data QuuxE =
    QuuxE1 Int
  | QuuxE2

pattern QuuxEP :: Int -> QuuxE
pattern QuuxEP n = QuuxE1 n

commonFunc :: Double -> Double
commonFunc x = x + x * x

$([d|
  derivedE :: Int -> Int
  derivedE x = x
  |])

