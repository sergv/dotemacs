----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Logging.DiscardLogs
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Saturday, 24 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.Logging.DiscardLogs
  ( DiscardLogsT
  , runDiscardLogsT
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Control

import Control.Monad.Logging
import Control.Monad.Filesystem

newtype DiscardLogsT m a = DiscardLogsT { runDiscardLogsT :: m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadBase b
    , MonadError e
    , MonadThrow
    , MonadCatch
    , MonadFS
    )

instance MonadTrans DiscardLogsT where
  lift = DiscardLogsT

instance MonadTransControl DiscardLogsT where
  type StT DiscardLogsT a = a
  liftWith f = DiscardLogsT $ f runDiscardLogsT
  restoreT   = DiscardLogsT

instance MonadBaseControl b m => MonadBaseControl b (DiscardLogsT m) where
  type StM (DiscardLogsT m) a = ComposeSt DiscardLogsT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

instance Monad m => MonadLog (DiscardLogsT m) where
  logDoc _ _ = pure ()
