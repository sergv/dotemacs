----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ErrorExcept
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.ErrorExcept
  ( ErrorExceptT
  , runErrorExceptT
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Writer

import Control.Monad.Filesystem (MonadFS)
import Control.Monad.Logging (MonadLog)

newtype ErrorExceptT e m a = ErrorExceptT { unErrorExceptT :: m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader r
    , MonadState s
    , MonadWriter w
    , MonadBase b
    , MonadCatch
    , MonadMask
    , MonadThrow
    , MonadLog
    )

deriving instance (MonadBaseControl IO m, MonadMask m) => MonadFS (ErrorExceptT e m)

instance MonadTrans (ErrorExceptT e) where
  {-# INLINE lift #-}
  lift = ErrorExceptT

instance MonadTransControl (ErrorExceptT e) where
  type StT (ErrorExceptT e) a = a
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}
  liftWith f = ErrorExceptT $ f unErrorExceptT
  restoreT   = ErrorExceptT

instance MonadBaseControl b m => MonadBaseControl b (ErrorExceptT e m) where
  type StM (ErrorExceptT e m) a = StM m (StT (ErrorExceptT e) a)
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM     #-}
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

instance (Exception e, MonadThrow m, MonadCatch m, MonadBase IO m) => MonadError e (ErrorExceptT e m) where
  {-# INLINE throwError #-}
  throwError = throwM
  catchError action handler = ErrorExceptT $
    catch (unErrorExceptT action) (unErrorExceptT . handler)

runErrorExceptT :: (Exception e, MonadCatch m) => ErrorExceptT e m a -> m (Either e a)
runErrorExceptT (ErrorExceptT action) =
  handle (pure . Left) $ Right <$> action
