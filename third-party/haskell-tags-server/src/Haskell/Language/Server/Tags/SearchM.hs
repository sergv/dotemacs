----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.SearchM
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 23 August 2016
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Haskell.Language.Server.Tags.SearchM
  ( SearchT
  , runSearchT
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.ErrorExcept
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control

import Control.Monad.Filesystem (MonadFS)
import Control.Monad.Logging (MonadLog)
import Data.ErrorMessage
import Haskell.Language.Server.Tags.Types

-- | Monad for carrying out symbol search operations.
newtype SearchT m a = SearchM (ErrorExceptT ErrorMessage (StateT TagsServerState (ReaderT TagsServerConf m)) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState TagsServerState
    , MonadReader TagsServerConf
    , MonadLog
    , MonadBase b
    )

deriving instance (MonadBase IO m, MonadCatch m) => MonadError ErrorMessage (SearchT m)
deriving instance (MonadBaseControl IO m, MonadMask m) => MonadFS (SearchT m)

runSearchT
  :: MonadCatch m
  => TagsServerConf
  -> TagsServerState
  -> SearchT m a
  -> m (Either ErrorMessage a, TagsServerState)
runSearchT conf serverState (SearchM action)
  = flip runReaderT conf
  $ flip runStateT serverState
  $ runErrorExceptT action
