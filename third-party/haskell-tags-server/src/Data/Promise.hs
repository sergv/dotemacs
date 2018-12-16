----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promise
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Sunday, 14 August 2016
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Data.Promise
  ( Promise
  , newPromise
  , getPromisedValue
  , putValue
  ) where

import Control.Concurrent.MVar
import Control.Monad.Base

-- | Promise to return some result. Once result becomes available, it stays so.
newtype Promise a = Promise { unPromise :: MVar a }

{-# INLINE newPromise #-}
-- | Create empty promise
newPromise :: MonadBase IO m => m (Promise a)
newPromise = liftBase $ Promise <$> newEmptyMVar

{-# INLINE getPromisedValue #-}
-- | Obtain value from a promise, if it's available. Block until value becomes
-- available.
getPromisedValue :: MonadBase IO m => Promise a -> m a
getPromisedValue = liftBase . readMVar . unPromise

{-# INLINE putValue #-}
-- | Add value to a promise, thus fulfilling the promise. This should be done
-- only once, or thread puting the value would block indefinitely.
putValue :: MonadBase IO m => Promise a -> a -> m ()
putValue (Promise var) = liftBase . putMVar var

