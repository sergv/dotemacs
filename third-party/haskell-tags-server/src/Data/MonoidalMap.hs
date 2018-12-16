----------------------------------------------------------------------------
-- |
-- Module      :  Data.MonoidalMap
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 28 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wredundant-constraints          #-}
{-# OPTIONS_GHC -Wsimplifiable-class-constraints #-}

module Data.MonoidalMap
  ( MonoidalMap
  , singleton
  , unMonoidalMap
  , lookup
  , findWithDefault
  ) where

import Prelude hiding (lookup)

import Data.Coerce
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

newtype MonoidalMap k v = MonoidalMap { unMonoidalMap :: Map k v }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Ord k, Semigroup v) => Semigroup (MonoidalMap k v) where
  {-# INLINE (<>) #-}
  (<>) (MonoidalMap x) (MonoidalMap y) = MonoidalMap $ M.unionWith (<>) x y

instance (Ord k, Semigroup v) => Monoid (MonoidalMap k v) where
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}
  mempty = MonoidalMap mempty
  mappend = (<>)

{-# INLINE singleton #-}
singleton :: forall k v. k -> v -> MonoidalMap k v
singleton = coerce (M.singleton :: k -> v -> Map k v)

{-# INLINE lookup #-}
lookup :: forall k v. Ord k => k -> MonoidalMap k v -> Maybe v
lookup = coerce (M.lookup :: k -> Map k v -> Maybe v)

{-# INLINE findWithDefault #-}
findWithDefault :: forall k v. Ord k => v -> k -> MonoidalMap k v -> v
findWithDefault = coerce (M.findWithDefault :: v -> k -> Map k v -> v)
