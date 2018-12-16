----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Input
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  19 June 2017
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Lexer.Input
  ( AlexInput
  , aiInput
  , aiInputL
  , aiPrevChar
  , aiBytes
  , aiLine
  , mkAlexInput
  , retrieveToken
  -- * Alex interface
  , alexInputPrevChar
  , alexGetByte
  ) where

import Codec.Binary.UTF8.String (encodeChar)
import Control.Applicative ((<|>))
import Data.Char
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Lens.Micro

import Haskell.Language.Lexer.FastTags
import Haskell.Language.Lexer.InputStack (InputStack(..), InputType(..))
import qualified Haskell.Language.Lexer.InputStack as InputStack

-- | Type that represents current position in the analyzed module.
data AlexInput = AlexInput
  { aiInput    :: InputStack
  , aiPrevChar :: {-# UNPACK #-} !Char
  , aiBytes    :: [Word8]
  , aiLine     :: {-# UNPACK #-} !Line
  } deriving (Eq, Ord, Show)

{-# INLINE aiInputL #-}
aiInputL :: Lens' AlexInput InputStack
aiInputL = lens aiInput (\s a -> s { aiInput = a })

mkAlexInput :: Text -> AlexInput
mkAlexInput s = AlexInput
  { aiInput    = OriginalSourceStack s'
  , aiPrevChar = '\n'
  , aiBytes    = []
  , aiLine     = initLine
  }
  where
    -- Line numbering starts from 0 because we're adding additional newline
    -- at the beginning to simplify processing. Thus, line numbers in the
    -- result are 1-based.
    initLine = Line 0

    s' = addLeadingNewline $ addTrailingNewline $ stripBOM s
    addLeadingNewline :: Text -> Text
    addLeadingNewline = T.cons '\n'
    addTrailingNewline :: Text -> Text
    addTrailingNewline str
      | T.null str         = "\n"
      | T.last str == '\n' = str
      | otherwise          = T.snoc str '\n'
    stripBOM :: Text -> Text
    stripBOM xs =
      fromMaybe xs $
      T.stripPrefix utf8BOM xs <|> T.stripPrefix utf16BOM xs <|> T.stripPrefix utf16BOM' xs
    utf16BOM  = "\xFFEF"
    utf16BOM' = "\xFEFF"
    utf8BOM   = "\xEF\xBB\xBF"

retrieveToken :: AlexInput -> Int -> Text
retrieveToken input len = InputStack.take len $ aiInput input

-- Alex interface
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiPrevChar

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input =
  case aiBytes input of
    b:bs -> Just (b, input { aiBytes = bs })
    []   -> nextChar
  where
    nextChar :: Maybe (Word8, AlexInput)
    nextChar = case InputStack.uncons $ aiInput input of
      Nothing           -> Nothing
      Just (typ, c, cs) -> Just $ encode typ (fromMaybe c $ fixChar c) cs
    encode :: InputType -> Char -> InputStack -> (Word8, AlexInput)
    encode typ c cs =
      case encodeChar c of
        b:bs -> (b, input')
          where
            input' = input
              { aiInput    = cs
              , aiBytes    = bs
              , aiPrevChar = c
              , aiLine     = advanceLine typ c $ aiLine input
              }
        []   -> error
          "alexGetByte: should not happen - utf8 encoding of a character is empty"

advanceLine :: InputType -> Char -> Line -> Line
advanceLine OriginalSource '\n' x = increaseLine x
advanceLine _              _    x = x

-- Translate unicode character into special symbol we teached Alex to recognize.
fixChar :: Char -> Maybe Char
-- These should not be translated since Alex known about them
fixChar '→' = Nothing
fixChar '∷' = Nothing
fixChar '⇒' = Nothing
fixChar '∀' = Nothing
fixChar c
  | c <= '\x7f' = Nothing -- Plain ascii needs no fixing.
  | otherwise
  = case generalCategory c of
      UppercaseLetter       -> Just upper
      LowercaseLetter       -> Just lower
      TitlecaseLetter       -> Just upper
      ModifierLetter        -> Just suffix
      OtherLetter           -> Just lower
      DecimalNumber         -> Just digit
      OtherNumber           -> Just digit
      ConnectorPunctuation  -> Just symbol
      DashPunctuation       -> Just symbol
      OtherPunctuation      -> Just symbol
      MathSymbol            -> Just symbol
      CurrencySymbol        -> Just symbol
      ModifierSymbol        -> Just symbol
      OtherSymbol           -> Just symbol
      Space                 -> Just space
      _other                -> Nothing
  where
    space  = '\x01'
    upper  = '\x02'
    lower  = '\x03'
    symbol = '\x04'
    digit  = '\x05'
    suffix = '\x06'

