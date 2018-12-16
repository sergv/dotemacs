----------------------------------------------------------------------------
-- |
-- Module      :  Definitions
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Definitions
  ( ViaMacroWithWildcardChildren(..)
  , ViaMacroWithExplicitChildren(MWEC2)
  , ViaMacroNoChildren
  , viaMacro
  , ViaDefWithWildcardChildren(..)
  , ViaDefWithExplicitChildren(DWEC2)
  , ViaDefNoChildren
  , viaDef
  ) where

#define NEW_TYPE(NAME, C1, C2) \
  data NAME = C1 Int | C2 Bool \
    deriving (Eq, Ord, Show)

#define NEW_FUNC(NAME, TYP) \
  NAME :: TYP -> TYP ; \
  NAME x = x

NEW_TYPE(ViaMacroWithWildcardChildren, MWWC1, MWWC2)
NEW_TYPE(ViaMacroWithExplicitChildren, MWEC1, MWEC2)
NEW_TYPE(ViaMacroNoChildren, MNC1, MNC2)

data ViaDefWithWildcardChildren =
    DWWC1 Int
  | DWWC2 Bool
  deriving (Eq, Ord, Show)

data ViaDefWithExplicitChildren =
    DWEC1 Int
  | DWEC2 Bool
  deriving (Eq, Ord, Show)

data ViaDefNoChildren =
    DNC1 Int
  | DNC2 Bool
  deriving (Eq, Ord, Show)

NEW_FUNC(viaMacro, Int)

viaDef :: a -> a
viaDef = id
