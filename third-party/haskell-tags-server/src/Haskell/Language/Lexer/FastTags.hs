----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.FastTags
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  20 June 2017
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Haskell.Language.Lexer.FastTags
  ( PragmaType(..)
  , ServerToken(..)
  , tokToName
  , stripNewlines
  , processTokens
  , stripServerTokens
  , embedServerToken
  , removeDuplicatePatterns
  , FastTags.TokenVal
  , module FastTags.Token
  , module FastTags.Tag
  ) where

import Control.Arrow (second)
import Control.DeepSeq

import Data.Binary
import Data.Either
import Data.Hashable
import Data.IgnoreEqOrdHashNFData
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Ext
import Data.Void (Void)
import GHC.Generics (Generic)

import qualified FastTags.Tag as FastTags
import qualified FastTags.Token as FastTags
import FastTags.Tag
  ( Pos(..)
  , TagVal(..)
  , Type(..)
  , breakBlocks
  , whereBlock
  , UnstrippedTokens(..)
  , unstrippedTokensOf
  )
import FastTags.Token (Line(..), Offset(..), SrcPos(..), increaseLine, posFile, posLine, unLine)

data PragmaType = SourcePragma
  deriving (Show, Eq, Ord, Generic)

instance Hashable PragmaType
instance NFData   PragmaType

instance Pretty PragmaType where
  pretty = ppGeneric


data ServerToken =
    Pragma !PragmaType
  | HSC2HS  -- Any HSC2HS directive
  | LBanana -- Arrows: (|
  | RBanana -- Arrows: |)
  | Error (IgnoreEqOrdHashNFData (Doc Void))

  | KWCase
  | KWClass
  | KWData
  | KWDefault
  | KWDeriving
  | KWDo
  | KWElse
  | KWFamily
  | KWForeign
  | KWIf
  | KWImport
  | KWIn
  | KWInfix
  | KWInfixl
  | KWInfixr
  | KWInstance
  | KWLet
  | KWModule
  | KWNewtype
  | KWOf
  | KWThen
  | KWType
  | KWWhere
  | Arrow
  | At
  | Backtick
  | Comma
  | Dot
  | DoubleColon
  | Equals
  | ExclamationMark
  | Implies
  | LBrace
  | LBracket
  | LParen
  | Pipe
  | RBrace
  | RBracket
  | RParen
  | Tilde
  | Semicolon
  | T {-# UNPACK  #-} !Text
  -- | Special token, not part of Haskell spec. Stores indentation.
  | Newline {-# UNPACK  #-} !Int
  -- | String contents is not tracked since it's irrelevant.
  | String
  -- | Actual character not tracked since it's irrelevant.
  | Character
  -- | Actual value not tracked since it's irrelevant.
  | Number
  | QuasiquoterStart
  | QuasiquoterEnd
  | SpliceStart -- \$(
  | LambdaBackslash -- \
  | EOF
  deriving (Eq, Ord, Show, Generic)

instance Hashable ServerToken

instance NFData ServerToken where

instance Pretty ServerToken where
  pretty = ppGeneric



deriving instance Generic FastTags.TokenVal
instance Hashable FastTags.TokenVal
instance NFData   FastTags.TokenVal

instance Pretty FastTags.TokenVal where
  pretty = ppGeneric

deriving instance Generic  Line
deriving instance Binary   Line
deriving instance Hashable Line
deriving instance Pretty   Line

instance Pretty SrcPos where
  pretty SrcPos{posFile, posLine} = pretty posFile <> ":" <> pretty (unLine posLine)

deriving instance Generic Type
instance Binary   Type
instance Hashable Type

instance Pretty Type where
  pretty = ppGeneric

deriving instance Generic FastTags.TagVal
instance Hashable FastTags.TagVal

tokToName :: ServerToken -> Maybe Text
tokToName ExclamationMark = Just "!"
tokToName Tilde           = Just "~"
tokToName Dot             = Just "."
tokToName (T "_")         = Nothing
tokToName (T name)        = Just name
tokToName _               = Nothing

stripNewlines :: [Pos ServerToken] -> [Pos ServerToken]
stripNewlines = filter isNonNewline
  where
    isNonNewline (Pos _ (Newline _)) = False
    isNonNewline _                   = True

stripServerTokens :: [Pos ServerToken] -> ([Pos FastTags.TokenVal], [Doc Void])
stripServerTokens = second catMaybes . partitionEithers . map f
  where
    f :: Pos ServerToken -> Either (Pos FastTags.TokenVal) (Maybe (Doc Void))
    f (Pos p t) = case t of
      Pragma _         -> Right Nothing
      HSC2HS           -> Right Nothing
      LBanana          -> Right Nothing
      RBanana          -> Right Nothing
      Error msg        -> Right $ Just $ unIgnoreEqOrdHashNFData msg
      KWCase           -> Left $ Pos p FastTags.KWCase
      KWClass          -> Left $ Pos p FastTags.KWClass
      KWData           -> Left $ Pos p FastTags.KWData
      KWDefault        -> Left $ Pos p FastTags.KWDefault
      KWDeriving       -> Left $ Pos p FastTags.KWDeriving
      KWDo             -> Left $ Pos p FastTags.KWDo
      KWElse           -> Left $ Pos p FastTags.KWElse
      KWFamily         -> Left $ Pos p FastTags.KWFamily
      KWForeign        -> Left $ Pos p FastTags.KWForeign
      KWIf             -> Left $ Pos p FastTags.KWIf
      KWImport         -> Left $ Pos p FastTags.KWImport
      KWIn             -> Left $ Pos p FastTags.KWIn
      KWInfix          -> Left $ Pos p FastTags.KWInfix
      KWInfixl         -> Left $ Pos p FastTags.KWInfixl
      KWInfixr         -> Left $ Pos p FastTags.KWInfixr
      KWInstance       -> Left $ Pos p FastTags.KWInstance
      KWLet            -> Left $ Pos p FastTags.KWLet
      KWModule         -> Left $ Pos p FastTags.KWModule
      KWNewtype        -> Left $ Pos p FastTags.KWNewtype
      KWOf             -> Left $ Pos p FastTags.KWOf
      KWThen           -> Left $ Pos p FastTags.KWThen
      KWType           -> Left $ Pos p FastTags.KWType
      KWWhere          -> Left $ Pos p FastTags.KWWhere
      Arrow            -> Left $ Pos p FastTags.Arrow
      At               -> Left $ Pos p FastTags.At
      Backtick         -> Left $ Pos p FastTags.Backtick
      Comma            -> Left $ Pos p FastTags.Comma
      Dot              -> Left $ Pos p FastTags.Dot
      DoubleColon      -> Left $ Pos p FastTags.DoubleColon
      Equals           -> Left $ Pos p FastTags.Equals
      ExclamationMark  -> Left $ Pos p FastTags.ExclamationMark
      Implies          -> Left $ Pos p FastTags.Implies
      LBrace           -> Left $ Pos p FastTags.LBrace
      LBracket         -> Left $ Pos p FastTags.LBracket
      LParen           -> Left $ Pos p FastTags.LParen
      Pipe             -> Left $ Pos p FastTags.Pipe
      RBrace           -> Left $ Pos p FastTags.RBrace
      RBracket         -> Left $ Pos p FastTags.RBracket
      RParen           -> Left $ Pos p FastTags.RParen
      Tilde            -> Left $ Pos p FastTags.Tilde
      Semicolon        -> Left $ Pos p FastTags.Semicolon
      T x              -> Left $ Pos p $ FastTags.T x
      Newline n        -> Left $ Pos p $ FastTags.Newline n
      String           -> Left $ Pos p FastTags.String
      Character        -> Left $ Pos p FastTags.Character
      Number           -> Left $ Pos p FastTags.Number
      QuasiquoterStart -> Left $ Pos p FastTags.QuasiquoterStart
      QuasiquoterEnd   -> Left $ Pos p FastTags.QuasiquoterEnd
      SpliceStart      -> Left $ Pos p FastTags.SpliceStart
      LambdaBackslash  -> Left $ Pos p FastTags.LambdaBackslash
      EOF              -> Left $ Pos p FastTags.EOF

embedServerToken :: FastTags.TokenVal -> ServerToken
embedServerToken = \case
  FastTags.KWCase           -> KWCase
  FastTags.KWClass          -> KWClass
  FastTags.KWData           -> KWData
  FastTags.KWDefault        -> KWDefault
  FastTags.KWDeriving       -> KWDeriving
  FastTags.KWDo             -> KWDo
  FastTags.KWElse           -> KWElse
  FastTags.KWFamily         -> KWFamily
  FastTags.KWForeign        -> KWForeign
  FastTags.KWIf             -> KWIf
  FastTags.KWImport         -> KWImport
  FastTags.KWIn             -> KWIn
  FastTags.KWInfix          -> KWInfix
  FastTags.KWInfixl         -> KWInfixl
  FastTags.KWInfixr         -> KWInfixr
  FastTags.KWInstance       -> KWInstance
  FastTags.KWLet            -> KWLet
  FastTags.KWModule         -> KWModule
  FastTags.KWNewtype        -> KWNewtype
  FastTags.KWOf             -> KWOf
  FastTags.KWThen           -> KWThen
  FastTags.KWType           -> KWType
  FastTags.KWWhere          -> KWWhere
  FastTags.Arrow            -> Arrow
  FastTags.At               -> At
  FastTags.Backtick         -> Backtick
  FastTags.Comma            -> Comma
  FastTags.Dot              -> Dot
  FastTags.DoubleColon      -> DoubleColon
  FastTags.Equals           -> Equals
  FastTags.ExclamationMark  -> ExclamationMark
  FastTags.Implies          -> Implies
  FastTags.LBrace           -> LBrace
  FastTags.LBracket         -> LBracket
  FastTags.LParen           -> LParen
  FastTags.Pipe             -> Pipe
  FastTags.RBrace           -> RBrace
  FastTags.RBracket         -> RBracket
  FastTags.RParen           -> RParen
  FastTags.Tilde            -> Tilde
  FastTags.Semicolon        -> Semicolon
  FastTags.T x              -> T x
  FastTags.Newline n        -> Newline n
  FastTags.String           -> String
  FastTags.Character        -> Character
  FastTags.Number           -> Number
  FastTags.QuasiquoterStart -> QuasiquoterStart
  FastTags.QuasiquoterEnd   -> QuasiquoterEnd
  FastTags.SpliceStart      -> SpliceStart
  FastTags.LambdaBackslash  -> LambdaBackslash
  FastTags.EOF              -> EOF

processTokens :: [Pos ServerToken] -> ([Pos FastTags.TagVal], [Doc Void])
processTokens toks
  = second ((errs ++) . map docFromString)
  $ FastTags.processTokens toks'
  where
    (toks', errs) = stripServerTokens toks

-- | Keep only one Pattern tag for each unique name.
removeDuplicatePatterns :: [Pos FastTags.TagVal] -> [Pos FastTags.TagVal]
removeDuplicatePatterns = go mempty
  where
    go :: Map Text SrcPos -> [Pos FastTags.TagVal] -> [Pos FastTags.TagVal]
    go !acc []     =
      map (\(name, pos) -> Pos pos (TagVal name Pattern Nothing)) $ M.toList acc
    go !acc (t:ts) =
      case t of
        Pos pos TagVal{tvName, tvType = Pattern, tvParent = Nothing} ->
          go (M.insertWith minPos tvName pos acc) ts
        t' -> t' : go acc ts

minPos :: SrcPos -> SrcPos -> SrcPos
minPos p1@SrcPos{posLine = l1} p2@SrcPos{posLine = l2}
  | l1 < l2   = p1
  | otherwise = p2
