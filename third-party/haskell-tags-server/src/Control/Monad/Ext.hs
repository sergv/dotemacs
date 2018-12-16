----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Ext
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 12 October 2016
----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Ext
  ( partitionM
  , partitionIO
  ) where

import Data.Foldable
import Data.DList (DList)
import qualified Data.DList as DL

import System.Unsafe (interleaveIO)

partitionM :: forall m a. Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM predicate = go mempty mempty
  where
    go :: DList a -> DList a -> [a] -> m ([a], [a])
    go xs ys []       = pure (toList xs, toList ys)
    go xs ys (z : zs) = do
      res <- predicate z
      if res
      then go (DL.snoc xs z) ys             zs
      else go xs             (DL.snoc ys z) zs

{-# INLINE partitionIO #-}
partitionIO :: forall a. (a -> IO Bool) -> [a] -> IO ([a], [a])
partitionIO predicate = go
  where
    go :: [a] -> IO ([a], [a])
    go []       = pure ([], [])
    go (z : zs) = do
      res <- predicate z
      ~(xs', ys') <- interleaveIO $ go zs
      pure $
        if res
        then (z : xs', ys')
        else (xs',     z : ys')
