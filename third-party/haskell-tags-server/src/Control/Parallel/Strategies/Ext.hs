----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel.Strategies.Ext
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Control.Parallel.Strategies.Ext
  ( foldPar
  , foldParNE
  , module Control.Parallel.Strategies
  ) where

import Control.Parallel.Strategies

import Data.List.NonEmpty (NonEmpty(..))

{-# INLINE foldPar #-}
-- | Reduce argument in a tree-like fashion.
--
-- NB only safe when used with commutative monoids.
foldPar :: Monoid a => [a] -> Eval a
foldPar = \case
  []     -> pure mempty
  x : xs -> foldParNE $ x :| xs

{-# INLINE foldParNE #-}
-- | Reduce argument in a tree-like fashion.
--
-- NB only safe when used with commutative monoids.
foldParNE :: Semigroup a => NonEmpty a -> Eval a
foldParNE (a :| as) = go [] a as
  where
    go []  x []         = pure x
    go acc x []         = go [] x acc
    go acc x [y1]       = do
      z <- rpar $ x <> y1
      go [] z acc
    go acc x (y1:y2:ys) = do
      z <- rpar $ x <> y1
      go (z : acc) y2 ys
