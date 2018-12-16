----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.TokenisationUtils
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Lexer.TokenisationUtils
  ( filename
  , testFullTagsWithoutPrefixes
  , testTagNames
  , untag
  , tokenize'
  , stripServerTokens'
  , module Haskell.Language.Lexer.FastTags
  ) where

import Test.Tasty

import Control.Arrow ((***))

import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Prettyprint.Doc.Ext as PP
import Data.Void (Void)
import GHC.Stack.Ext (WithCallStack)

import Haskell.Language.Lexer (LiterateLocation(..))
import qualified Haskell.Language.LexerSimple.Lexer as Lexer
import TestUtils (makeTest)

import Haskell.Language.Lexer.FastTags
  ( PragmaType(..)
  , ServerToken(..)
  , TokenVal
  , TagVal(..)
  , Pos(..)
  , Type(..)
  , SrcPos(..)
  , Line(..)
  , breakBlocks
  , whereBlock
  , processTokens
  , UnstrippedTokens(..)
  , unstrippedTokensOf
  , stripServerTokens
  , embedServerToken
  )

filename :: FilePath
filename = "/foo/bar/fn.hs"

testFullTagsWithoutPrefixes
  :: WithCallStack
  => FilePath -> LiterateLocation Void -> T.Text -> [Pos TagVal] -> TestTree
testFullTagsWithoutPrefixes fn mode = \source tags ->
  makeTest ((sort *** map PP.displayDocString) . processTokens . tokenize' fn mode) source (tags, warnings)
  where
    warnings :: [String]
    warnings = []

testTagNames
  :: WithCallStack
  => FilePath -> LiterateLocation Void -> T.Text -> [String] -> TestTree
testTagNames fn mode source tags =
  makeTest process source (tags, warnings)
  where
    warnings :: [String]
    warnings = []

    process :: T.Text -> ([String], [String])
    process =
      (sort . map untag *** map PP.displayDocString) . processTokens . tokenize' fn mode

untag :: Pos TagVal -> String
untag (Pos _ (TagVal name _ _)) = T.unpack name

tokenize'
  :: WithCallStack
  => FilePath -> LiterateLocation Void -> T.Text -> [Pos ServerToken]
tokenize' fn mode =
    -- either (error . PP.displayDocString . PP.pretty) id
  -- . runIdentity
  -- . Lexer.tokenizeM fn mode
  -- .
  Lexer.tokenize fn mode . TE.encodeUtf8

stripServerTokens' :: [Pos ServerToken] -> [Pos TokenVal]
stripServerTokens' ts =
  case stripServerTokens ts of
    (ts', [])       -> ts'
    (_,   es@(_:_)) -> error $ PP.displayDocString $
      PP.ppFoldableHeaderWith id "Errors while stripping server tokens:" es

-- tokenize''
--   :: FilePath
--   -> LiterateLocation Void
--   -> [(PathFragment, Text)]
--   -> Text
--   -> [Token]
-- tokenize'' fn mode includes =
--   either (error . show) id . runIdentity . Lexer.tokenizeM fn mode
