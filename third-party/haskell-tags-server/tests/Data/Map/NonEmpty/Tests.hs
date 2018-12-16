----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map.NonEmpty.Tests
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_GHC -w #-}

module Data.Map.NonEmpty.Tests (tests) where

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap

tests :: TestTree
tests = testGroup "Data.Map.NonEmpty"
  [ QC.testProperty "∀ k v m: NEMap.lookup k (RT.insert k v m) == v" $
    \(m :: NonEmptyMap OrdA B) (k :: OrdA) (v :: B) ->
      NEMap.lookup k (NEMap.insert k v m) == Just v
  , QC.testProperty "∀ xs: NEMap.fromList xs == M.fromList xs" $
    \(xs :: NonEmpty (OrdA, B)) ->
      toList (NEMap.toNonEmpty (NEMap.fromNonEmpty xs)) == M.toAscList (M.fromList (toList xs))

  , QC.testProperty "∀ xs ys: NEMap.union xs ys == M.union xs ys" $
    \(xs :: NonEmpty (OrdA, B)) ->
      (NEMap.union ===== M.union) xs
  , QC.testProperty "∀ f xs ys: NEMap.unionWith f xs ys == M.unionWith f xs ys" $
    \(f :: Fun (B, B) B) (xs :: NonEmpty (OrdA, B)) ->
      (NEMap.unionWith (curry (applyFun f)) ===== M.unionWith (curry (applyFun f))) xs
  ]

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary
  shrink xs =
    [ y :| ys
    | (y:ys) <- shrink $ toList xs
    ]

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (NonEmptyMap k v) where
  arbitrary = NEMap.fromNonEmpty <$> arbitrary
  shrink    = map NEMap.fromNonEmpty . shrink . NEMap.toNonEmpty

(=====)
  :: (Ord k, Eq v)
  => (NonEmptyMap k v -> NonEmptyMap k v -> NonEmptyMap k v)
  -> (Map k v -> Map k v -> Map k v)
  -> NonEmpty (k, v)
  -> NonEmpty (k, v)
  -> Bool
(=====) f g xs ys =
  toList (NEMap.toNonEmpty (f (NEMap.fromNonEmpty xs) (NEMap.fromNonEmpty ys))) == M.toAscList (g (M.fromList (toList xs)) (M.fromList (toList ys)))

