----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Par.Combinator.Ext
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Par.Combinator.Ext
  ( parMapLazy
  , parMapLazyM
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Par.Class

newtype Lazy a = Lazy { unLazy :: a }

instance NFData (Lazy a) where
  rnf x = x `seq` ()

{-# INLINE parMapLazy #-}
parMapLazy :: forall t iv p a b. (Traversable t, ParFuture iv p) => (a -> b) -> t a -> p (t b)
parMapLazy f = traverse (spawnP . Lazy . f) >=> traverse (pure . unLazy <=< get)

{-# INLINE parMapLazyM #-}
parMapLazyM :: forall t iv p a b. (Traversable t, ParFuture iv p) => (a -> p b) -> t a -> p (t b)
parMapLazyM f = traverse (spawnP . Lazy <=< f) >=> traverse (pure . unLazy <=< get)
