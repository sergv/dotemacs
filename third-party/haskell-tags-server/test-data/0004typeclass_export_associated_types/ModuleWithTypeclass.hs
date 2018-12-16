----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithTypeclass
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 20 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

module ModuleWithTypeclass
  ( Test(..)
  , TestFam(..)
  ) where

class Test a where
  data TestFam a :: *
  data PrivateFam a :: *
  wrap   :: a -> TestFam a
  unwrap :: TestFam a -> a
  toPrivate :: TestFam a -> PrivateFam a
  fromPrivate :: PrivateFam a -> TestFam a

instance Test Int where
  data TestFam Int = IntBox
    { unIntBox :: Int }
    deriving (Eq, Ord, Show)
  data PrivateFam Int = IntBoxPrivate
    { unIntBoxPrivate :: Int }
    deriving (Eq, Ord, Show)
  wrap   = IntBox
  unwrap = unIntBox
  toPrivate = IntBoxPrivate . unIntBox
  fromPrivate = IntBox . unIntBoxPrivate
