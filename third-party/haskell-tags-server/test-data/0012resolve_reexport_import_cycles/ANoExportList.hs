----------------------------------------------------------------------------
-- |
-- Module      :  ANoExportList
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module ANoExportList where

data FooA = FooA1
  { fooA1 :: Int
  , fooA2 :: !Double
  }

newtype BarA =
  BarA1
    { unBarA :: [Double] }

pattern BazAP :: Double -> Double -> BarA
pattern BazAP x y = BarA1 [x, y]

quuxA :: Int -> Int
quuxA x = x

pattern FrobAP :: Int -> FooA
pattern FrobAP x = FooA1 { fooA1 = x, fooA2 = 0 }

data QuuxA =
    QuuxA1 Int
  | QuuxA2

pattern QuuxAP :: Int -> QuuxA
pattern QuuxAP n = QuuxA1 n

commonFunc :: Double -> Double
commonFunc x = x + x * x

$([d|
  derivedA :: Int -> Int
  derivedA x = x
  |])
