----------------------------------------------------------------------------
-- |
-- Module      :  Data.NBSem
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Data.NBSem
  ( NBSem
  , newNBSem
  , tryAcquireNBSem
  , releaseNBSem
  ) where

import Control.Monad.Base
import Data.IORef

newtype NBSem = NBSem (IORef Int)

{-# INLINE newNBSem #-}
newNBSem :: MonadBase IO m => Int -> m NBSem
newNBSem i = liftBase $ NBSem <$> newIORef i

{-# INLINE tryAcquireNBSem #-}
tryAcquireNBSem :: MonadBase IO m => NBSem -> m Bool
tryAcquireNBSem (NBSem m) = liftBase $
  atomicModifyIORef' m $ \i ->
    if i == 0
    then (i, False)
    else let !z = i - 1 in (z, True)

{-# INLINE releaseNBSem #-}
releaseNBSem :: MonadBase IO m => NBSem -> m ()
releaseNBSem (NBSem m) = liftBase $
  atomicModifyIORef m $ \i ->
    let !z = i + 1 in (z, ())
