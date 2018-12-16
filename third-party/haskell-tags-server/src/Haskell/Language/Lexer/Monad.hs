----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Monad
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  21 June 2017
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haskell.Language.Lexer.Monad
  ( -- * Alex monad
    AlexT
  , runAlexT
  ) where

import Control.Monad.EitherCPS
import Control.Monad.Except.Ext
import Control.Monad.Reader
import Control.Monad.State
import Data.Text (Text)

import Data.ErrorMessage
import Haskell.Language.Lexer.Env
import Haskell.Language.Lexer.Input
import Haskell.Language.Lexer.State
import Haskell.Language.Lexer.Types

newtype AlexT m a = AlexT (EitherCPST ErrorMessage (ReaderT AlexEnv (StateT AlexState m)) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError ErrorMessage
    , MonadReader AlexEnv
    , MonadState AlexState
    )

runAlexT
  :: Monad m
  => FilePath
  -> LiterateMode
  -> AlexCode
  -> AlexCode
  -> Text
  -> AlexT m a
  -> m (Either ErrorMessage a)
runAlexT filename mode code toplevelCode input (AlexT action) =
  flip evalStateT s $
  flip runReaderT env $
  runEitherCPST action (pure . Left) (pure . Right)
  where
    s :: AlexState
    s   = mkAlexState (mkAlexInput input) code toplevelCode
    env :: AlexEnv
    env = mkAlexEnv filename mode

