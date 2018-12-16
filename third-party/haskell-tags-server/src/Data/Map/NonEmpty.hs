----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map.NonEmpty
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Monday, 24 October 2016
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}

{-# OPTIONS_GHC -Wredundant-constraints          #-}
{-# OPTIONS_GHC -Wsimplifiable-class-constraints #-}

module Data.Map.NonEmpty
  ( NonEmptyMap
  , singleton
  , lookup
  , member
  , insert
  , insertWith
  , delete
  , fromNonEmpty
  , toNonEmpty
  , keysNE
  , elemsNE
  , union
  , unionWith
  , difference
  , differenceWith
  ) where

import Data.Binary
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Semigroup
import GHC.Generics
import Prelude hiding (lookup)

-- | A map that always contains at least one key-value pair.
data NonEmptyMap k v =
  -- Invariant: map never contains root key stored in the constructor itself.
  -- @k@ is always the smallest key.
  NonEmptyMap !k !v !(Map k v)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (Binary k, Binary v) => Binary (NonEmptyMap k v)

instance (Ord k, Semigroup v) => Semigroup (NonEmptyMap k v) where
  {-# INLINE (<>) #-}
  (<>) = unionWith (<>)

{-# INLINE singleton #-}
singleton :: k -> v -> NonEmptyMap k v
singleton k v = NonEmptyMap k v M.empty

{-# INLINE lookup #-}
lookup :: Ord k => k -> NonEmptyMap k v -> Maybe v
lookup k (NonEmptyMap k' v' m)
  | k == k'   = Just v'
  | otherwise = M.lookup k m

{-# INLINE member #-}
member :: Ord k => k -> NonEmptyMap k v -> Bool
member k (NonEmptyMap k' _ m)
  | k == k'   = True
  | otherwise = M.member k m

{-# INLINE insert #-}
insert :: Ord k => k -> v -> NonEmptyMap k v -> NonEmptyMap k v
insert = insertWith const

{-# INLINE insertWith #-}
insertWith
  :: Ord k
  => (v -> v -> v) -> k -> v -> NonEmptyMap k v -> NonEmptyMap k v
insertWith f k v (NonEmptyMap k' v' m) =
  case compare k k' of
    LT -> NonEmptyMap k v $ M.insertWith f k' v' m
    EQ -> NonEmptyMap k (f v v') m
    GT -> NonEmptyMap k' v' $ M.insertWith f k v m

{-# INLINE delete #-}
delete :: Ord k => k -> NonEmptyMap k v -> Maybe (NonEmptyMap k v)
delete k (NonEmptyMap k' v' m)
  | k == k'   =
    case M.minViewWithKey m of
      Nothing               -> Nothing
      Just ((k'', v''), m') -> Just $ NonEmptyMap k'' v'' m'
  | otherwise = Just $ NonEmptyMap k' v' $ M.delete k m

{-# INLINE fromNonEmpty #-}
fromNonEmpty :: Ord k => NonEmpty (k, v) -> NonEmptyMap k v
fromNonEmpty kvs =
  case M.minViewWithKey $ M.fromList $ toList kvs of
    Nothing ->
      error "impossible: map with non-empty number of elements has no minimum"
    Just ((k', v'), m) -> NonEmptyMap k' v' m

{-# INLINE toNonEmpty #-}
toNonEmpty :: NonEmptyMap k v -> NonEmpty (k, v)
toNonEmpty (NonEmptyMap k v m) = (k, v) :| M.toList m

{-# INLINE keysNE #-}
keysNE :: NonEmptyMap k v -> NonEmpty k
keysNE (NonEmptyMap k _ m) = k :| M.keys m

{-# INLINE elemsNE #-}
elemsNE :: NonEmptyMap k v -> NonEmpty v
elemsNE (NonEmptyMap _ v m) = v :| M.elems m

{-# INLINE union #-}
union :: Ord k => NonEmptyMap k v -> NonEmptyMap k v -> NonEmptyMap k v
union = unionWith const

{-# INLINE unionWith #-}
unionWith
  :: Ord k
  => (v -> v -> v)
  -> NonEmptyMap k v
  -> NonEmptyMap k v
  -> NonEmptyMap k v
unionWith f (NonEmptyMap k1 v1 m1) (NonEmptyMap k2 v2 m2) =
  case compare k1 k2 of
    LT -> NonEmptyMap k1 v1          $ M.unionWith f m1 (M.insert k2 v2 m2)
    EQ -> NonEmptyMap k1 (v1 `f` v2) $ M.unionWith f m1 m2
    GT -> NonEmptyMap k2 v2          $ M.unionWith f (M.insert k1 v1 m1) m2

{-# INLINE difference #-}
difference
  :: Ord k
  => NonEmptyMap k a
  -> NonEmptyMap k b
  -> Maybe (NonEmptyMap k a)
difference = differenceWith (\_ _ -> Nothing)

{-# INLINE differenceWith #-}
differenceWith
  :: Ord k
  => (a -> b -> Maybe a)
  -> NonEmptyMap k a
  -> NonEmptyMap k b
  -> Maybe (NonEmptyMap k a)
differenceWith f (NonEmptyMap k1 v1 m1) (NonEmptyMap k2 v2 m2) =
  case M.minViewWithKey $ M.differenceWith f (M.insert k1 v1 m1) (M.insert k2 v2 m2) of
    Nothing          -> Nothing
    Just ((k, v), m) -> Just $ NonEmptyMap k v m
