----------------------------------------------------------------------------
-- |
-- Module      :  Data.SymbolMap
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 27 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{-# OPTIONS_GHC -Wredundant-constraints          #-}
{-# OPTIONS_GHC -Wsimplifiable-class-constraints #-}

module Data.SymbolMap
  ( SymbolMap
  , null
  , insert
  , registerChildren
  , lookup
  , lookupChildren
  , member
  , isSubsetNames
  , fromList
  , toList
  , restrictKeys
  , withoutKeys
  , keysSet
  , childrenRelations
  ) where

import Prelude hiding (lookup, null)

import Control.Arrow ((&&&), second)
import Control.DeepSeq

import Data.Binary
import qualified Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)

import Data.Symbols
import Data.Text.Prettyprint.Doc.Ext

data SymbolMap = SymbolMap
  { -- | Map from children entities to parents containing them. E.g.
    -- constructors are mapped to their corresponding datatypes, typeclass
    -- members - to typeclass names, etc.
    --
    -- Even though each child has one and only one parent, when taking union
    -- of two unrelated modules it may occur that child would be assigned to
    -- different parents. This should be prevented by careful analysis of
    -- module headers, but, unfortunately, cannot be ruled out entirely because
    -- we're not a Haskell compiler.
    smParentMap   :: !(Map UnqualifiedSymbolName (Set UnqualifiedSymbolName))
    -- | Map from parents to children.
  , smChildrenMap :: !(Map UnqualifiedSymbolName (Set UnqualifiedSymbolName))
  , smAllSymbols  :: !(Map UnqualifiedSymbolName (NonEmpty ResolvedSymbol))
  } deriving (Eq, Ord, Show, Generic)

instance Binary SymbolMap
instance NFData SymbolMap

instance Semigroup SymbolMap where
  {-# INLINE (<>) #-}
  SymbolMap x y z <> SymbolMap x' y' z' = SymbolMap
    { smParentMap   = M.unionWith (<>) x x'
    , smChildrenMap = M.unionWith (<>) y y'
    , smAllSymbols  = M.unionWith (Semigroup.<>) z z'
    }

instance Monoid SymbolMap where
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}
  mempty  = SymbolMap mempty mempty mempty
  mappend = (<>)

instance Pretty SymbolMap where
  pretty SymbolMap{smParentMap, smChildrenMap, smAllSymbols} = ppDictHeader "SymbolMap"
    [ "ParentMap"   :-> ppMapWith pretty ppSet smParentMap
    , "ChildrenMap" :-> ppMapWith pretty ppSet smChildrenMap
    , "AllSymbols"  :-> ppMapWith pretty ppNE  smAllSymbols
    ]

{-# INLINE null #-}
null :: SymbolMap -> Bool
null SymbolMap{smParentMap, smChildrenMap, smAllSymbols} =
  M.null smAllSymbols && M.null smParentMap && M.null smChildrenMap

insert :: ResolvedSymbol -> SymbolMap -> SymbolMap
insert sym m = SymbolMap
  { smParentMap   = parentMap
  , smChildrenMap = childrenMap
  , smAllSymbols  = M.alter (addToNE sym) name $ smAllSymbols m
  }
  where
    name :: UnqualifiedSymbolName
    name = resolvedSymbolName sym
    (parentMap, childrenMap) = case resolvedSymbolParent sym of
      Nothing -> (smParentMap m, smChildrenMap m)
      Just p  ->
        ( M.alter (addToSet p)    name $ smParentMap m
        , M.alter (addToSet name) p    $ smChildrenMap m
        )
    addToNE :: a -> Maybe (NonEmpty a) -> Maybe (NonEmpty a)
    addToNE x = \case
      Nothing -> Just $ x :| []
      Just xs -> Just $ NE.cons x xs
    addToSet :: Ord a => a -> Maybe (Set a) -> Maybe (Set a)
    addToSet x = \case
      Nothing -> Just $ S.singleton x
      Just xs -> Just $ S.insert x xs

-- | Add extra child-parent relationships to a symbol map.
registerChildren
  :: Map UnqualifiedSymbolName (Set UnqualifiedSymbolName) -- ^ Map from parents to children
  -> SymbolMap
  -> SymbolMap
registerChildren extraChildrenMap SymbolMap{smParentMap, smChildrenMap, smAllSymbols} = SymbolMap
  { smParentMap   = M.unionWith (<>) smParentMap extraParents
  , smChildrenMap = M.unionWith (<>) smChildrenMap extraChildrenMap'
  , smAllSymbols  = smAllSymbols
  }
  where
    extraChildrenMap' :: Map UnqualifiedSymbolName (Set UnqualifiedSymbolName)
    extraChildrenMap' =
      (`S.intersection` smAllSymbolsKeys) <$> (extraChildrenMap `M.intersection` smAllSymbols)
    extraParents :: Map UnqualifiedSymbolName (Set UnqualifiedSymbolName)
    extraParents
      = M.fromListWith (<>)
      $ concatMap (\(parent, children) -> map (\c -> (c, S.singleton parent)) $ Data.Foldable.toList children)
      $ M.toList extraChildrenMap'
    smAllSymbolsKeys :: Set UnqualifiedSymbolName
    smAllSymbolsKeys = M.keysSet smAllSymbols

{-# INLINE lookup #-}
lookup :: UnqualifiedSymbolName -> SymbolMap -> Maybe (NonEmpty ResolvedSymbol)
lookup sym = M.lookup sym . smAllSymbols

{-# INLINE _lookupParent #-}
-- Noone uses it yet, so it's hidden here in case it will be needed later.
_lookupParent :: UnqualifiedSymbolName -> SymbolMap -> Maybe (Set UnqualifiedSymbolName)
_lookupParent sym = M.lookup sym . smParentMap

{-# INLINE lookupChildren #-}
lookupChildren :: UnqualifiedSymbolName -> SymbolMap -> Maybe (Set UnqualifiedSymbolName)
lookupChildren sym = M.lookup sym . smChildrenMap

{-# INLINE member #-}
member :: UnqualifiedSymbolName -> SymbolMap -> Bool
member sym = M.member sym . smAllSymbols

{-# INLINE isSubsetNames #-}
isSubsetNames :: Set UnqualifiedSymbolName -> SymbolMap -> Bool
isSubsetNames xs = (xs `S.isSubsetOf`) . keysSet

fromList :: [ResolvedSymbol] -> SymbolMap
fromList syms = SymbolMap
  { smParentMap   = M.fromListWith (<>) $
      map (second S.singleton) symbolsWithParents
  , smChildrenMap = M.fromListWith (<>) $
      map (\(child, parent) -> (parent, S.singleton child)) symbolsWithParents
  , smAllSymbols  = M.fromListWith (<>) $
      map (resolvedSymbolName &&& (:| [])) syms
  }
  where
    symbolsWithParents :: [(UnqualifiedSymbolName, UnqualifiedSymbolName)]
    symbolsWithParents =
      mapMaybe (\sym -> (resolvedSymbolName sym,) <$> resolvedSymbolParent sym) syms

{-# INLINE toList #-}
toList :: SymbolMap -> [ResolvedSymbol]
toList = foldMap Data.Foldable.toList . smAllSymbols

restrictKeys :: SymbolMap -> Set UnqualifiedSymbolName -> SymbolMap
restrictKeys SymbolMap{smParentMap, smChildrenMap, smAllSymbols} syms =
  SymbolMap
    { smParentMap   = (`S.intersection` syms) <$> (smParentMap   `M.restrictKeys` syms)
    , smChildrenMap = (`S.intersection` syms) <$> (smChildrenMap `M.restrictKeys` syms)
    , smAllSymbols  = smAllSymbols `M.restrictKeys` syms
    }

withoutKeys :: SymbolMap -> Set UnqualifiedSymbolName -> SymbolMap
withoutKeys SymbolMap{smParentMap, smChildrenMap, smAllSymbols} syms =
  SymbolMap
    { smParentMap   = (S.\\ syms) <$> (smParentMap   `M.withoutKeys` syms)
    , smChildrenMap = (S.\\ syms) <$> (smChildrenMap `M.withoutKeys` syms)
    , smAllSymbols  = smAllSymbols `M.withoutKeys` syms
    }

{-# INLINE keysSet #-}
keysSet :: SymbolMap -> Set UnqualifiedSymbolName
keysSet = M.keysSet . smAllSymbols

childrenRelations :: SymbolMap -> [(ResolvedSymbol, [ResolvedSymbol])]
childrenRelations SymbolMap{smChildrenMap, smAllSymbols} =
  foldMap
    (\(parent, children) -> map (\p -> (p, foldMap resolve children)) $ resolve parent)
    (M.toList smChildrenMap)
  where
    resolve :: UnqualifiedSymbolName -> [ResolvedSymbol]
    resolve = maybe [] Data.Foldable.toList . (`M.lookup` smAllSymbols)
