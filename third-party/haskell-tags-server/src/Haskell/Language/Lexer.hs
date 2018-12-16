----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday,  3 November 2016
----------------------------------------------------------------------------

module Haskell.Language.Lexer
  ( tokenize
  -- , tokenizeM
  -- , LiterateMode(..)
  , LiterateLocation(..)
  ) where

-- import Data.Functor.Identity
import qualified Data.ByteString as BS
import GHC.Stack.Ext (WithCallStack)
import System.FilePath

-- import Haskell.Language.Lexer.FastTags (Token)
-- import Haskell.Language.Lexer.Lexer (tokenizeM)
-- import Haskell.Language.Lexer.Types (LiterateMode(..))

import Haskell.Language.Lexer.FastTags
import qualified Haskell.Language.LexerSimple.Lexer as SimpleLexer
import Haskell.Language.LexerSimple.Types (LiterateLocation(..))

tokenize :: WithCallStack => FilePath -> BS.ByteString -> [Pos ServerToken]
-- tokenize filename = runIdentity . tokenizeM filename mode
  -- where
  --   mode :: LiterateMode
  --   mode
  --     | takeExtension filename == ".lhs" = Literate
  --     | otherwise                        = Vanilla
tokenize filename = SimpleLexer.tokenize filename mode
  where
    mode :: LiterateLocation a
    mode
      | takeExtension filename == ".lhs" = LiterateOutside
      | otherwise                        = Vanilla
