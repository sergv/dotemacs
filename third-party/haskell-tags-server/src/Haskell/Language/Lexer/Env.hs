----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Env
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  20 June 2017
----------------------------------------------------------------------------

module Haskell.Language.Lexer.Env
  ( AlexEnv(..)
  , mkAlexEnv
  ) where

import Haskell.Language.Lexer.Types (LiterateMode)

-- | Environment for user rule predicates.
data AlexEnv = AlexEnv
  { aeFilename     :: FilePath
  , aeLiterateMode :: LiterateMode
  } deriving (Eq, Ord, Show)

mkAlexEnv :: FilePath -> LiterateMode -> AlexEnv
mkAlexEnv filename mode = AlexEnv
  { aeFilename     = filename
  , aeLiterateMode = mode
  }

