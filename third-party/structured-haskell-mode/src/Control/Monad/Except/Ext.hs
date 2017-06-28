----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Except.Ext
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  24 June 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts  #-}

module Control.Monad.Except.Ext
  ( throwErrorWithBacktrace
  , throwErrorWithBacktrace'
  , HasCallStack
  , module Control.Monad.Except
  ) where

import Control.Monad.Except
import qualified Data.Text.Lazy as TL
import GHC.Stack

import Data.ErrorMessage

throwErrorWithBacktrace
  :: (HasCallStack, MonadError ErrorMessage m) => TL.Text -> m a
throwErrorWithBacktrace = throwErrorWithBacktrace' id

throwErrorWithBacktrace'
  :: MonadError e m => (ErrorMessage -> e) -> TL.Text -> m a
throwErrorWithBacktrace' f = throwError . f . mkErrorMessage
