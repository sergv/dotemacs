----------------------------------------------------------------------------
-- |
-- Module      :  Data.IgnoreEqOrdHashNFData
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.IgnoreEqOrdHashNFData
  ( IgnoreEqOrdHashNFData(..)
  ) where

import Control.DeepSeq

import Data.Coerce
import Data.Hashable
import Data.Text.Prettyprint.Doc.Ext

newtype IgnoreEqOrdHashNFData a =
  IgnoreEqOrdHashNFData { unIgnoreEqOrdHashNFData :: a }
  deriving (Pretty)

instance NFData (IgnoreEqOrdHashNFData a) where
  {-# INLINE rnf #-}
  rnf x = x `seq` ()

instance Eq (IgnoreEqOrdHashNFData a) where
  {-# INLINE (==) #-}
  (==) _ _ = True

instance Ord (IgnoreEqOrdHashNFData a) where
  {-# INLINE compare #-}
  compare _ _ = EQ

instance Hashable (IgnoreEqOrdHashNFData a) where
  {-# INLINE hashWithSalt #-}
  hashWithSalt = const

instance Show a => Show (IgnoreEqOrdHashNFData a) where
  {-# INLINE showsPrec #-}
  showsPrec = coerce (showsPrec :: Int -> a -> ShowS)

instance {-# OVERLAPS #-} PPGenericOverride a => PPGenericOverride (IgnoreEqOrdHashNFData a) where
  ppGenericOverride = ppGenericOverride . unIgnoreEqOrdHashNFData

