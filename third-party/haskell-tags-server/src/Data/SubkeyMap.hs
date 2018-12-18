----------------------------------------------------------------------------
-- |
-- Module      :  Data.SubkeyMap
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Saturday,  8 October 2016
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wredundant-constraints          #-}
{-# OPTIONS_GHC -Wsimplifiable-class-constraints #-}

module Data.SubkeyMap
  ( SubkeyMap
  , HasSubkey(..)
  , empty
  , null
  , insert
  , insertWith
  , lookup
  , member
  , lookupSubkey
  , lookupSubkeyKeys
  , alter'
  , traverseWithKey
  , traverseMaybeWithKey
  , fromMap
  , fromList
  , fromFoldable
  , toMap
  , toSubmap
  , toList
  , toSubkeyList
  , toSubkeyKeyList
  , keys
  , restrictKeys
  , withoutKeys
  ) where

import Prelude hiding (lookup, null)

import Control.Arrow
import Control.DeepSeq

import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.Store (Store)
import GHC.Generics (Generic)

class (Ord k, Ord (Subkey k)) => HasSubkey k where
  type Subkey k :: *
  getSubkey :: k -> Subkey k

-- | Map which can index same set of values by two keys. One key is the
-- main one (the bigger one), the second key is the subkey which is a projection
-- of the main key. -- Since it is a projection, it may reference several
-- values.
-- Deletions are not provided for the time being in order to simplify
-- impementation.
-- Invariant: both keys are kept in sync with each other.
data SubkeyMap k v = SubkeyMap
  { smMainMap :: !(Map k v)
  , smSubMap  :: !(Map (Subkey k) (Set k))
  } deriving (Functor, Foldable, Traversable, Generic)

deriving instance (Eq   k,  Eq  (Subkey k), Eq   v) => Eq   (SubkeyMap k v)
deriving instance (Ord  k, Ord  (Subkey k), Ord  v) => Ord  (SubkeyMap k v)
deriving instance (Show k, Show (Subkey k), Show v) => Show (SubkeyMap k v)

instance (Ord k, Ord (Subkey k), Store k, Store (Subkey k), Store v) => Store (SubkeyMap k v)
instance (NFData k, NFData (Subkey k), NFData v) => NFData (SubkeyMap k v)

instance (HasSubkey k, Semigroup v) => Semigroup (SubkeyMap k v) where
  {-# INLINE (<>) #-}
  SubkeyMap m1 s1 <> SubkeyMap m2 s2 = SubkeyMap
    { smMainMap = M.unionWith (<>) m1 m2
    , smSubMap  = M.unionWith (<>) s1 s2
    }

instance (HasSubkey k, Semigroup v) => Monoid (SubkeyMap k v) where
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}
  mempty  = empty
  mappend = (Semigroup.<>)

{-# INLINE empty #-}
empty :: SubkeyMap k v
empty = SubkeyMap
  { smMainMap = M.empty
  , smSubMap  = M.empty
  }

{-# INLINE null #-}
null :: SubkeyMap k v -> Bool
null = M.null . smMainMap

{-# INLINE insert #-}
insert :: HasSubkey k => k -> v -> SubkeyMap k v -> SubkeyMap k v
insert = insertWith const

{-# INLINE insertWith #-}
insertWith :: HasSubkey k => (v -> v -> v) -> k -> v -> SubkeyMap k v -> SubkeyMap k v
insertWith f k v SubkeyMap{smMainMap, smSubMap} = SubkeyMap
  { smMainMap = M.insertWith f k v smMainMap
  , smSubMap  = M.insertWith (<>) (getSubkey k) (S.singleton k) smSubMap
  }

{-# INLINE lookup #-}
lookup :: Ord k => k -> SubkeyMap k v -> Maybe v
lookup k = M.lookup k . smMainMap

{-# INLINE member #-}
member :: Ord k => k -> SubkeyMap k v -> Bool
member k = M.member k . smMainMap

{-# INLINE lookupSubkey #-}
lookupSubkey :: HasSubkey k => Subkey k -> SubkeyMap k v -> [(k, v)]
lookupSubkey k SubkeyMap{smMainMap, smSubMap} =
  case M.lookup k smSubMap of
    Nothing   -> []
    Just idxs -> smMainMap `indexBySet` idxs

{-# INLINE lookupSubkeyKeys #-}
-- | Find out which keys correspond to the given subkey.
lookupSubkeyKeys :: HasSubkey k => Subkey k -> SubkeyMap k v -> Maybe (Set k)
lookupSubkeyKeys k = M.lookup k . smSubMap

{-# INLINE alter' #-}
alter' :: HasSubkey k => (Maybe v -> v) -> k -> SubkeyMap k v -> SubkeyMap k v
alter' f k SubkeyMap{smMainMap, smSubMap} = SubkeyMap
  { smMainMap = M.alter (Just . f) k smMainMap
  , smSubMap  = M.insertWith (<>) (getSubkey k) (S.singleton k) smSubMap
  }

{-# INLINE traverseWithKey #-}
traverseWithKey :: Applicative f => (k -> v -> f v') -> SubkeyMap k v -> f (SubkeyMap k v')
traverseWithKey f sm@SubkeyMap{smMainMap} =
  (\smMainMap' -> sm { smMainMap = smMainMap' }) <$> M.traverseWithKey f smMainMap

traverseMaybeWithKey
  :: forall f k v v'. (Applicative f, HasSubkey k)
  => (k -> v -> f (Maybe v')) -> SubkeyMap k v -> f (SubkeyMap k v')
traverseMaybeWithKey f sm@SubkeyMap{smMainMap, smSubMap} =
  update <$> M.traverseMaybeWithKey f smMainMap
  where
    update :: Map k v' -> SubkeyMap k v'
    update smMainMap' = sm
      { smMainMap = smMainMap'
      , smSubMap  =
        -- Do the expensive update only if anything changed.
        if M.size smMainMap' == M.size smMainMap
        then smSubMap
        else (`S.intersection` ks) <$> M.restrictKeys smSubMap subkeys
      }
      where
        ks :: Set k
        ks = M.keysSet smMainMap'
        subkeys :: Set (Subkey k)
        subkeys = S.map getSubkey ks

fromMap :: HasSubkey k => Map k v -> SubkeyMap k v
fromMap m = SubkeyMap
  { smMainMap = m
  , smSubMap  = M.fromListWith (<>) $ map (getSubkey &&& S.singleton) $ M.keys m
  }

{-# INLINE fromList #-}
fromList :: (HasSubkey k, Semigroup v) => [(k, v)] -> SubkeyMap k v
fromList = fromFoldable

fromFoldable :: (Foldable f, HasSubkey k, Semigroup v) => f (k, v) -> SubkeyMap k v
fromFoldable = foldl' (\acc (k, v) -> insertWith (<>) k v acc) empty

{-# INLINE toMap #-}
toMap :: SubkeyMap k v -> Map k v
toMap = smMainMap

{-# INLINE toSubmap #-}
toSubmap :: Ord k => SubkeyMap k v -> Map (Subkey k) [v]
toSubmap SubkeyMap{smMainMap, smSubMap} =
  map snd . (smMainMap `indexBySet`) <$> smSubMap

{-# INLINE toList #-}
toList :: SubkeyMap k v -> [(k, v)]
toList = M.toList . toMap

{-# INLINE toSubkeyList #-}
toSubkeyList :: Ord k => SubkeyMap k v -> [(Subkey k, [v])]
toSubkeyList = M.toList . toSubmap

{-# INLINE toSubkeyKeyList #-}
toSubkeyKeyList :: SubkeyMap k v -> [(Subkey k, Set k)]
toSubkeyKeyList = M.toList . smSubMap

{-# INLINE keys #-}
keys :: SubkeyMap k v -> [k]
keys = M.keys . smMainMap

-- | Leav only keys found in a Set within a Map.
restrictKeys :: forall k v. HasSubkey k => SubkeyMap k v -> Set k -> SubkeyMap k v
restrictKeys SubkeyMap{smMainMap, smSubMap} ks =
  SubkeyMap
    { smMainMap = M.restrictKeys smMainMap ks
    , smSubMap  = (`S.intersection` ks) <$> M.restrictKeys smSubMap  subkeys
    }
  where
    subkeys :: Set (Subkey k)
    subkeys = S.map getSubkey ks

-- | Remove all keys found in a Set from a Map.
withoutKeys :: forall k v. HasSubkey k => SubkeyMap k v -> Set k -> SubkeyMap k v
withoutKeys SubkeyMap{smMainMap, smSubMap} ks =
  SubkeyMap
    { smMainMap =               M.withoutKeys smMainMap ks
    , smSubMap  = (S.\\ ks) <$> M.withoutKeys smSubMap  subkeys
    }
  where
    subkeys :: Set (Subkey k)
    subkeys = S.map getSubkey ks

-- Utils

{-# INLINE indexBySet #-}
indexBySet :: Ord k => Map k v -> Set k -> [(k, v)]
indexBySet m ixs = M.toList $ M.restrictKeys m ixs
