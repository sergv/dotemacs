----------------------------------------------------------------------------
-- |
-- Module      :  Data.Condition
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Monday, 12 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wredundant-constraints          #-}
{-# OPTIONS_GHC -Wsimplifiable-class-constraints #-}

module Data.Condition
  ( Condition
  , newUnsetCondition
  , setCondition
  , waitForCondition
  ) where

import Control.Concurrent.MVar
import Control.Monad.Base

-- | Concurrent condition that can be awaited to become true.
newtype Condition = Condition (MVar ())
  deriving (Eq)

{-# INLINE newUnsetCondition #-}
newUnsetCondition :: MonadBase IO m => m Condition
newUnsetCondition = liftBase $ Condition <$> newEmptyMVar

{-# INLINE setCondition #-}
setCondition :: MonadBase IO m => Condition -> m ()
setCondition (Condition v) = liftBase $ putMVar v ()

{-# INLINE waitForCondition #-}
waitForCondition :: MonadBase IO m => Condition -> m ()
waitForCondition (Condition v) = liftBase $ readMVar v
