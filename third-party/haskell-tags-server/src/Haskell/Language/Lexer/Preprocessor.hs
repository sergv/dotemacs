----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Preprocessor
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  27 May 2017
----------------------------------------------------------------------------

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Haskell.Language.Lexer.Preprocessor
  ( PreprocessorMacro(..)
  , ConstantMacroDef(..)
  , FunctionMacroDef(..)
  , defaultFunctionMacroDef
  , isConstant
  , isFunction
  , extractConstant
  , extractFunction
  , parsePreprocessorDefine
  , parsePreprocessorUndef
  ) where

import Control.Monad.Except.Ext
import Data.Attoparsec.Text
import Data.Char (isAlpha, isDigit)
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Ext (Pretty(..))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Ext as PP

import Data.ErrorMessage
import Data.KeyMap (HasKey)
import qualified Data.KeyMap as KM
import Data.Symbols.MacroName

data ConstantMacroDef = ConstantMacroDef
  { cmdName :: !MacroName
  , cmdBody :: !Text
  } deriving (Eq, Ord, Show)

instance Pretty ConstantMacroDef where
  pretty ConstantMacroDef{cmdName, cmdBody} =
    PP.hsep ["#define", pretty cmdName, pretty cmdBody]

data FunctionMacroDef = FunctionMacroDef
  { fmdName :: !MacroName
  , fmdArgs :: ![MacroName]
  , fmdBody :: !Text
  } deriving (Eq, Ord, Show)

defaultFunctionMacroDef :: FunctionMacroDef
defaultFunctionMacroDef = FunctionMacroDef
  { fmdName = mkMacroName mempty
  , fmdArgs = []
  , fmdBody = mempty
  }

instance Pretty FunctionMacroDef where
  pretty FunctionMacroDef{fmdName, fmdArgs, fmdBody} =
    PP.hsep
      [ "#define"
      , pretty fmdName
      , PP.parens $ PP.hsep $ map ((<> ",") . pretty) fmdArgs
      , pretty fmdBody
      ]

data PreprocessorMacro =
    PreprocessorConstant ConstantMacroDef
  | PreprocessorFunction FunctionMacroDef
  deriving (Eq, Ord, Show)

macroName :: PreprocessorMacro -> MacroName
macroName = \case
  PreprocessorConstant def -> cmdName def
  PreprocessorFunction def -> fmdName def

instance HasKey PreprocessorMacro where
  type Key PreprocessorMacro = MacroName
  {-# INLINE getKey #-}
  getKey = macroName

instance Pretty PreprocessorMacro where
  pretty = \case
    PreprocessorConstant def -> pretty def
    PreprocessorFunction def -> pretty def

isConstant :: PreprocessorMacro -> Bool
isConstant = isJust . extractConstant

isFunction :: PreprocessorMacro -> Bool
isFunction = isJust . extractFunction

extractConstant :: PreprocessorMacro -> Maybe ConstantMacroDef
extractConstant = \case
  PreprocessorConstant def -> Just def
  PreprocessorFunction{}   -> Nothing

extractFunction :: PreprocessorMacro -> Maybe FunctionMacroDef
extractFunction = \case
  PreprocessorConstant{}   -> Nothing
  PreprocessorFunction def -> Just def

-- | Parse "#define ..." directive
parsePreprocessorDefine
  :: (WithCallStack, MonadError ErrorMessage m)
  => Text
  -> m PreprocessorMacro
parsePreprocessorDefine =
  either (throwErrorWithCallStack . PP.docFromString) pure . parseOnly (pDefine <* endOfInput)

-- | Parse "#undef ..." directive
parsePreprocessorUndef
  :: (WithCallStack, MonadError ErrorMessage m)
  => Text
  -> m MacroName
parsePreprocessorUndef =
  either (throwErrorWithCallStack . PP.docFromString) pure . parseOnly (pUndef <* endOfInput)

-- | Parse preprocessor directive start - hash, followed by optional whitespace,
-- and literal directive name.
pDirectiveStart :: Text -> Parser ()
pDirectiveStart directive = do
  _ <- char '#'    <?> "hash"
  skipMany pCppWS  <?> "optional whitespace after hash"
  void (string directive) <?> T.unpack directive

pDefine :: Parser PreprocessorMacro
pDefine = do
  pDirectiveStart "define"
  skipMany1 pCppWS       <?> "mandatory whitespace after define"
  name <- pCppIdentifier <?> "defined name"
  args <- option Nothing (Just <$> pArguments <?> "arguments")
  skipMany1 pCppWS       <?> "space before macro body"
  body <- pCppBody       <?> "macro body"
  let name' = mkMacroName name
      body' :: PreprocessorMacro
      body' = case args of
        Nothing    -> PreprocessorConstant ConstantMacroDef
          { cmdName = name'
          , cmdBody = body
          }
        Just args' -> PreprocessorFunction FunctionMacroDef
          { fmdName = name'
          , fmdArgs = args'
          , fmdBody = body
          }
  pure body'

pUndef :: Parser MacroName
pUndef = do
  pDirectiveStart "undef"
  skipMany1 pCppWS        <?> "mandatory whitespace after define"
  ident <- pCppIdentifier <?> "name"
  pure $ mkMacroName ident

pCppIdentifier :: Parser Text
pCppIdentifier =
  T.cons
    <$> ((satisfy isLeadingCPPIdentifierChar <?> "first cpp identifier char")
        <* skipMany pContinuationLine)
    <*> (T.concat <$> (takeWhile1 isCPPIdentifierChar `sepBy` pContinuationLine <?> "rest of cpp identifier"))
  where
    pContinuationLine = char '\\' *> skipNewline <?> "continuation line"

-- Characters that can occur at first position of Cpp identifier.
isLeadingCPPIdentifierChar :: Char -> Bool
isLeadingCPPIdentifierChar c = case c of
  '_'  -> True
  '\'' -> True
  '`'  -> True
  c    -> isAlpha c

isCPPIdentifierChar :: Char -> Bool
isCPPIdentifierChar c = isLeadingCPPIdentifierChar c || isDigit c

pArguments :: Parser [MacroName]
pArguments = do
  _    <- char '('
  skipMany pCppWS
  args <- (mkMacroName <$> pCppIdentifier <* skipMany pCppWS) `sepBy` (char ',' *> skipMany pCppWS)
  skipMany pCppWS
  _    <- char ')'
  pure args

pCppBody :: Parser Text
pCppBody = stripTrailingNewline . removeComments . removeContinuationMarkers <$> takeText
  where
    removeContinuationMarkers =
      T.replace "\\\r" "" . T.replace "\\\n" "" . T.replace "\\\r\n" ""
    removeComments xs =
      case T.splitOn "/*" xs of
        []   -> T.empty -- no occurrences of /*
        [y]  -> y       -- no occurrences of /*
        y:ys -> T.concat $ y : map (T.drop 2 . snd . T.breakOn "*/") ys
    stripTrailingNewline = T.dropWhileEnd isAsciiSpaceOrNewline

-- pCppBody = T.concat <$> sepBy1 takeMany
-- pCppBody = takeText
--   c <- anyChar
--   case c of
--     '\\' -> undefined
--     '\r' -> undefined
--     '\n' -> undefined

pCppWS :: Parser ()
pCppWS = do
  c <- anyChar
  case c of
    '\\'                 -> skipNewline
    c' | isAsciiSpace c' -> pure ()
    _                    -> fail "pCppWS"
  -- char '\\' *> skipNewline <|> skip isAsciiSpace

skipNewline :: Parser ()
skipNewline = do
  c <- anyChar
  case c of
    '\r' -> skip isNewline
    '\n' -> pure ()
    _    -> fail "skipNewline"
  where
    isNewline :: Char -> Bool
    isNewline = \case
      '\n' -> True
      _    -> False

isAsciiSpace :: Char -> Bool
isAsciiSpace = \case
  ' '  -> True
  '\t' -> True
  '\r' -> True
  _    -> False

isAsciiSpaceOrNewline :: Char -> Bool
isAsciiSpaceOrNewline = \case
  ' '  -> True
  '\t' -> True
  '\r' -> True
  '\n' -> True
  '\f' -> True
  '\v' -> True
  _    -> False

