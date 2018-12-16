----------------------------------------------------------------------------
-- |
-- Module      :  DependencyShiftedConstructorsTypes
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module DependencyShiftedConstructorsTypes where


data FooShifted = QuuxShifted Int Double

newtype BarShifted =
  FooShifted { unBarShifted :: String }

data BazShifted =
    Hello
  | BarShifted

data QuuxShifted a ix where
  BazShifted  :: a      -> QuuxShifted a a
  QuuxDouble  :: Double -> QuuxShifted a Double
