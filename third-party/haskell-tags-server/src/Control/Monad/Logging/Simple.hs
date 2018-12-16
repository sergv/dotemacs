----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Logging.Simple
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 30 August 2016
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.Logging.Simple
  ( SimpleLoggerT
  , Severity(..)
  , Destination(..)
  , runSimpleLoggerT
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Coerce
import qualified Data.Text.Lazy.IO as TLIO
import Data.Void (Void)
import System.IO

import Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
import Data.Text.Prettyprint.Doc.Ext

data SimpleLoggerCfg m = SimpleLoggerCfg
  { logSink     :: Doc Void -> m ()
  , logSeverity :: Severity
  }

newtype SimpleLoggerT m a = SimpleLoggerT
  { unSimpleLoggerT :: ReaderT (SimpleLoggerCfg m) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadBase b
    , MonadError e
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadIO
    )

instance MonadTrans SimpleLoggerT where
  lift = SimpleLoggerT . lift

-- instance MonadTransControl SimpleLoggerT where
--   type StT SimpleLoggerT a = StT (ReaderT (SimpleLoggerCfg m)) a

instance MonadBaseControl n m => MonadBaseControl n (SimpleLoggerT m) where
  type StM (SimpleLoggerT m) a = StM (ReaderT (SimpleLoggerCfg m) m) a
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM     #-}
  liftBaseWith f = SimpleLoggerT $ liftBaseWith (\g -> f (g . unSimpleLoggerT))
  restoreM :: forall a. StM (SimpleLoggerT m) a -> SimpleLoggerT m a
  restoreM = coerce . (restoreM :: StM (ReaderT (SimpleLoggerCfg m) m) a -> ReaderT (SimpleLoggerCfg m) m a)

deriving instance (MonadBaseControl IO m, MonadMask m) => MonadFS (SimpleLoggerT m)

instance Monad m => MonadLog (SimpleLoggerT m) where
  logDoc severity msg = SimpleLoggerT $ do
    SimpleLoggerCfg{logSink, logSeverity} <- ask
    when (severity >= logSeverity) $
      lift $ logSink msg

data Destination m where
  Stderr :: MonadBase IO m =>                       Destination m
  Stdout :: MonadBase IO m =>                       Destination m
  Custom ::                   (Doc Void -> m ()) -> Destination m

runSimpleLoggerT
  :: forall m a. Applicative m
  => Maybe (Destination m)
  -> Severity
  -> SimpleLoggerT m a
  -> m a
runSimpleLoggerT dest severity (SimpleLoggerT action) =
  runReaderT action cfg
  where
    cfg :: SimpleLoggerCfg m
    cfg = SimpleLoggerCfg
      { logSink     =
          case dest of
            Nothing    -> \_ -> pure ()
            Just dest' ->
              case dest' of
                Stderr   -> \msg -> liftBase $ TLIO.hPutStrLn stderr (displayDoc msg) *> hFlush stderr
                Stdout   -> \msg -> liftBase $ TLIO.hPutStrLn stdout (displayDoc msg) *> hFlush stdout
                Custom f -> f
      , logSeverity = severity
      }
