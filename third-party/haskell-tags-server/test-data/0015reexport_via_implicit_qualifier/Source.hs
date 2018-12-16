----------------------------------------------------------------------------
-- |
-- Module      :  Source
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Source where

foo :: Int -> Int
foo = id

newtype Bar = Bar { unBar :: Double }
