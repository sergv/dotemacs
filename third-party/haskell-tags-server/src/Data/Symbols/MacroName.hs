----------------------------------------------------------------------------
-- |
-- Module      :  Data.Symbols.MacroName
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :   5 June 2017
----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Symbols.MacroName
  ( MacroName
  , mkMacroName
  , unMacroName
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Ext

newtype MacroName = MacroName { unMacroName :: Text }
  deriving (Eq, Ord, Pretty, Show)

mkMacroName :: Text -> MacroName
mkMacroName = MacroName

