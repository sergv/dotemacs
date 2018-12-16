----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Pair
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Strict.Pair
  ( Pair(..)
  ) where

data Pair a b = Pair !a !b

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  {-# INLINE (<>) #-}
  (<>) (Pair a b) (Pair a' b') = Pair (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}
  mempty = Pair mempty mempty
  mappend = (<>)
