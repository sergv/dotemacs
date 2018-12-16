----------------------------------------------------------------------------
-- |
-- Module      :  Imported1
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE TypeOperators #-}

module Imported1 where

foo :: a -> a
foo x = x

bar :: a -> a
bar x = x

($$) :: a -> a -> a
x $$ _ = x

data (:$$:) a b =
  (:$$$:) a b
