----------------------------------------------------------------------------
-- |
-- Module      :  PrefixFoo
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module PrefixFoo where

prefixFoo :: Int -> Int
prefixFoo x = if x > 0 then x else - x
