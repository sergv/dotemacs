----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.RulePredicate
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  19 June 2017
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Haskell.Language.Lexer.RulePredicate
  ( RulePredM
  , runRulePredM
  , runRulePredM'
  , inputBeforeToken
  , matchedNameInPredicate
  , isLiterate
  , isInBirdEnv
  , isInLatexCodeEnv
  , isNameDefinedAsConstant
  , isNameDefinedAsFunction
  , isNameDefinedAsMacroArgument
  ) where

import Control.Monad.Reader
-- import Data.HasLens
import Data.Profunctor
import Data.Text (Text)

import qualified Data.KeyMap as KM
import Data.Symbols.MacroName (MacroName)
import Haskell.Language.Lexer.Env
import Haskell.Language.Lexer.Input
import qualified Haskell.Language.Lexer.InputStack as InputStack
import Haskell.Language.Lexer.Preprocessor (isConstant, isFunction)
import Haskell.Language.Lexer.State
import Haskell.Language.Lexer.Types

newtype RulePredM r a = RulePredM
  { runRulePredM
      :: r         -- ^ Predicate state.
      -> AlexInput -- ^ Input stream before the token.
      -> Int       -- ^ Length of the token.
      -- -> AlexInput -- ^ Input stream after the token.
      -> a
  } deriving (Functor)

runRulePredM'
  :: RulePredM r a
  -> r         -- ^ Predicate state
  -> AlexInput -- ^ Input stream before the token.
  -> Int       -- ^ Length of the token
  -> AlexInput -- ^ Input stream after the token.
  -> a
runRulePredM'  (RulePredM f)state inputBefore len _inputAfter =
  f state inputBefore len

instance Applicative (RulePredM r) where
  pure x = RulePredM $ \_ _ _ -> x
  RulePredM gf <*> RulePredM gx =
    RulePredM $ \a b c -> gf a b c $ gx a b c

instance Monad (RulePredM r) where
  return = pure
  RulePredM gf >>= k =
    RulePredM $ \a b c -> runRulePredM (k (gf a b c)) a b c

instance MonadReader r (RulePredM r) where
  ask = RulePredM $ \x _ _ -> x
  local f (RulePredM g) = RulePredM $ \a -> g (f a)

instance Profunctor RulePredM where
  dimap f g (RulePredM action) =
    RulePredM $ \a b c -> g $ action (f a) b c

inputBeforeToken :: RulePredM a AlexInput
inputBeforeToken = RulePredM $ \_ input _ -> input

matchedNameInPredicate :: RulePredM a Text
matchedNameInPredicate =
  RulePredM $ \_ input len -> retrieveToken input len

isLiterate :: RulePredM AlexEnv Bool
isLiterate = do
  env <- ask
  pure $ case aeLiterateMode env of
    Literate -> True
    Vanilla  -> False

isInBirdEnv :: RulePredM AlexState Bool
isInBirdEnv = do
  style <- asks asLiterateStyle
  pure $ case style of
    Nothing    -> False
    Just Latex -> False
    Just Bird  -> True

isInLatexCodeEnv :: RulePredM AlexState Bool
isInLatexCodeEnv = do
  style <- asks asLiterateStyle
  pure $ case style of
    Nothing    -> False
    Just Latex -> True
    Just Bird  -> False

-- | Check whether given name is a cpp define.
isNameDefinedAsConstant :: MacroName -> RulePredM AlexState Bool
isNameDefinedAsConstant name =
  asks $ any (any isConstant) . KM.lookup name . asDefines

-- | Check whether given name is a cpp define of a macro with arguments.
isNameDefinedAsFunction :: MacroName -> RulePredM AlexState Bool
isNameDefinedAsFunction name =
  asks $ any (any isFunction) . KM.lookup name . asDefines

-- | Check whether given name is a cpp define.
isNameDefinedAsMacroArgument :: MacroName -> RulePredM a Bool
isNameDefinedAsMacroArgument name = do
  input <- inputBeforeToken
  pure $ case InputStack.lookupMacroArg name $ aiInput input of
    Nothing -> False
    Just _  -> True
