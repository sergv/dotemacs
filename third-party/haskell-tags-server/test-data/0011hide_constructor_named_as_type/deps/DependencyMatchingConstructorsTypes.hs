----------------------------------------------------------------------------
-- |
-- Module      :  DependencyMatchingConstructorsTypes
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module DependencyMatchingConstructorsTypes where

data FooMatching = FooMatching Int Double

newtype BarMatching =
  BarMatching { unBarMatching :: String }

data BazMatching =
    Hello
  | BazMatching

data QuuxMatching a ix where
  QuuxMatching :: a   -> QuuxMatching a a
  QuuxInt      :: Int -> QuuxMatching a Int
