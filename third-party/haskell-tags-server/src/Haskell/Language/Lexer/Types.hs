---------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Types
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday,  3 November 2016
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Haskell.Language.Lexer.Types
  ( mkSrcPos
  , Context(..)
  , LiterateMode(..)
  , AlexCode(..)
  , LiterateStyle(..)
  ) where

import Data.Text.Prettyprint.Doc.Ext (Pretty(..))

import Haskell.Language.Lexer.FastTags (Line, Offset(..), SrcPos(..))

{-# INLINE mkSrcPos #-}
mkSrcPos :: FilePath -> Line -> SrcPos
mkSrcPos filename line = SrcPos
  { posFile   = filename
  , posLine   = line
  , posPrefix = mempty
  , posOffset = Offset 0
  , posSuffix = mempty
  }

data LiterateMode = Literate | Vanilla
  deriving (Eq, Ord, Show)

data Context
  = CtxHaskell
  | CtxQuasiquoter
  deriving (Eq, Ord, Show)

-- | Abstract wrapper around alex automata states.
newtype AlexCode = AlexCode { unAlexCode :: Int }
  deriving (Eq, Ord, Show, Pretty, Enum, Num, Real, Integral)

data LiterateStyle = Bird | Latex
  deriving (Eq, Ord, Show, Enum, Bounded)
