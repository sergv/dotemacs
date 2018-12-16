----------------------------------------------------------------------------
-- |
-- Module      :  CWildcardExportListWithChildrenPlusSome
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module CWildcardExportListWithChildrenPlusSome
  ( FooC(.., FrobCP)
  , BarC(.., BazCP)
  , quuxC
  , QuuxC(.., QuuxCP)
  , commonFunc
  , derivedC
  ) where

data FooC = FooC1
  { fooC1 :: Int
  , fooC2 :: !Double
  }

newtype BarC =
  BarC1
    { unBarC :: [Double] }

pattern BazCP :: Double -> Double -> BarC
pattern BazCP x y = BarC1 [x, y]

quuxC :: Int -> Int
quuxC x = x

pattern FrobCP :: Int -> FooC
pattern FrobCP x = FooC1 { fooC1 = x, fooC2 = 0 }

data QuuxC =
    QuuxC1 Int
  | QuuxC2

pattern QuuxCP :: Int -> QuuxC
pattern QuuxCP n = QuuxC1 n

commonFunc :: Double -> Double
commonFunc x = x + x * x

$([d|
  derivedC :: Int -> Int
  derivedC x = x
  |])
