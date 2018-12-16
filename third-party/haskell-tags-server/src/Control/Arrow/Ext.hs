----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.Ext
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 12 October 2016
----------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}

module Control.Arrow.Ext (secondM) where

secondM :: Functor m => (b -> m c) -> (a, b) -> m (a, c)
secondM f (x, y) = (x,) <$> f y
