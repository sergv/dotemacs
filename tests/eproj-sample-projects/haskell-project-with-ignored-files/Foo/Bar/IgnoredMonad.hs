----------------------------------------------------------------------------
-- |
-- Module      :  Foo.Bar.IgnoredMonad
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Monday, 12 October 2015
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Foo.Bar.IgnoredMonad where

data IgnoredM a = IgnoredM

instance Monad IgnoredM where
    IgnoredM >>= _ = IgnoredM
    return _ = IgnoredM
