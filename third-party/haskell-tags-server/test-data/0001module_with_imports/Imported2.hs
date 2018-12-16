----------------------------------------------------------------------------
-- |
-- Module      :  Imported2
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE TypeOperators #-}

module Imported2 where

foo2 :: a -> a
foo2 x = x

bar2 :: a -> a
bar2 x = x

($$*) :: a -> a -> a
x $$* _ = x

data (:$$*:) a b =
  (:$$$*:) a b
