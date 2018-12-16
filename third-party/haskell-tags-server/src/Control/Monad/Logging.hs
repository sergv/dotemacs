----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Logging
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 24 August 2016
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Control.Monad.Logging
  ( MonadLog(..)
  , Severity(..)
  , showSeverity
  , readSeverity
  , knownSeverities
  , logError
  , logWarning
  , logInfo
  , logDebug
  , logVerboseDebug
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as SS
import Control.Monad.Writer as Lazy
import Control.Monad.Writer.Strict as Strict

import Data.Bimap (Bimap)
import qualified Data.Bimap as BM
import qualified Data.List as L
import Data.String
import Data.Text.Prettyprint.Doc (Doc)
import Data.Void (Void)

data Severity = VerboseDebug | Debug | Info | Warning | Error
  deriving (Eq, Ord, Show)

severities :: (Ord a, IsString a) => Bimap a Severity
severities = BM.fromList
  [ ("verbose-debug", VerboseDebug)
  , ("debug",         Debug)
  , ("info",          Info)
  , ("warning",       Warning)
  , ("error",         Error)
  ]

showSeverity :: Severity -> String
showSeverity = (severities BM.!>)

readSeverity :: MonadError String m => String -> m Severity
readSeverity str = case BM.lookup str severities of
  Nothing -> throwError $
    "Invalid verbosity: " ++ str ++
    ". Allowed values: " ++ L.intercalate ", " knownSeverities
  Just x  -> pure x

knownSeverities :: (Ord a, IsString a) => [a]
knownSeverities = BM.keys severities


class Monad m => MonadLog m where
  logDoc :: Severity -> Doc Void -> m ()

{-# INLINE logError #-}
logError :: MonadLog m => Doc Void -> m ()
logError = logDoc Error

{-# INLINE logWarning #-}
logWarning :: MonadLog m => Doc Void -> m ()
logWarning = logDoc Warning

{-# INLINE logInfo #-}
logInfo :: MonadLog m => Doc Void -> m ()
logInfo = logDoc Info

{-# INLINE logDebug #-}
logDebug :: MonadLog m => Doc Void -> m ()
logDebug = logDoc Debug

{-# INLINE logVerboseDebug #-}
logVerboseDebug :: MonadLog m => Doc Void -> m ()
logVerboseDebug = logDoc VerboseDebug

instance MonadLog m => MonadLog (ExceptT e m) where
  {-# INLINE logDoc #-}
  logDoc s = lift . logDoc s

instance MonadLog m => MonadLog (ReaderT r m) where
  {-# INLINE logDoc #-}
  logDoc s = lift . logDoc s

instance MonadLog m => MonadLog (StateT s m) where
  {-# INLINE logDoc #-}
  logDoc s = lift . logDoc s

instance (MonadLog m, Monoid w) => MonadLog (Lazy.WriterT w m) where
  {-# INLINE logDoc #-}
  logDoc s = lift . logDoc s

instance (MonadLog m, Monoid w) => MonadLog (Strict.WriterT w m) where
  {-# INLINE logDoc #-}
  logDoc s = lift . logDoc s

instance MonadLog m => MonadLog (SS.StateT s m) where
  {-# INLINE logDoc #-}
  logDoc s = lift . logDoc s
