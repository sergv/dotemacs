----------------------------------------------------------------------------
-- |
-- Module      :  Data.CompiledRegex
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 16 August 2016
----------------------------------------------------------------------------

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.CompiledRegex
  ( CompiledRegex
  , crSource
  , crRegex
  , compileRegex
  , compileRegexWithOpts
  , fileGlobsToRegex
  , reMatches
  , module Text.Regex.TDFA
  ) where

import Control.Monad.Except.Ext
import Data.Foldable
import Data.Function (on)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext
import Text.Regex.TDFA
import qualified Text.Regex.TDFA.Text as TDFA

import Data.ErrorMessage

-- | Wrapper around Regex that provides dummy Show, Eq and Ord instances
data CompiledRegex = CompiledRegex
  { crSource :: !Text
  , crRegex  :: !Regex
  }

instance Pretty CompiledRegex where
  pretty = ppShow

instance Show CompiledRegex where
  show CompiledRegex{crSource} = "CompiledRegex " ++ show crSource

instance Eq CompiledRegex where
  (==) = (==) `on` crSource

instance Ord CompiledRegex where
  compare = compare `on` crSource

compileRegex
  :: (WithCallStack, MonadError ErrorMessage m)
  => Text -> m CompiledRegex
compileRegex = compileRegexWithOpts opts
  where
    opts = defaultCompOpt
      { newSyntax      = True
      , multiline      = True
      , caseSensitive  = False
      , lastStarGreedy = True
      }

compileRegexWithOpts
  :: (WithCallStack, MonadError ErrorMessage m)
  => CompOption -> Text -> m CompiledRegex
compileRegexWithOpts compOpts src =
  case TDFA.compile compOpts' execOpts src of
    Left (err :: String) -> throwErrorWithCallStack $
      "Failed to compile regular expression" <+> PP.squotes (pretty src) <> ":" ## pretty err
    Right re -> pure CompiledRegex
      { crSource = src
      , crRegex  = re
      }
  where
    compOpts' :: CompOption
    compOpts' = compOpts
      {  lastStarGreedy = True -- We're not interested in capturing groups anywhere within tags server.
      }
    execOpts :: ExecOption
    execOpts = defaultExecOpt
      { captureGroups = False
      }

fileGlobsToRegex
  :: (WithCallStack, MonadError ErrorMessage m, Foldable f)
  => f Text -> m CompiledRegex
fileGlobsToRegex
  = compileRegexWithOpts compOpts
  . mkStartEnd
  . mkGroup
  . T.intercalate "|"
  . fmap (mkGroup . T.concatMap f)
  . toList
  where
    mkGroup :: Text -> Text
    mkGroup = T.cons '(' . (`T.snoc` ')')
    mkStartEnd :: Text -> Text
    mkStartEnd = T.cons '^' . (`T.snoc` '$')
    f :: Char -> Text
    f '*'  = ".*"
    f '.'  = "\\."
    f '+'  = "\\+"
    f '['  = "\\["
    f ']'  = "\\]"
    f '('  = "\\("
    f ')'  = "\\)"
    f '^'  = "\\^"
    f '$'  = "\\$"
    f '?'  = "\\?"
#ifdef mingw32_HOST_OS
    f '\\' = "[\\/]"
    f '/'  = "[\\/]"
#else
    f '\\' = "\\\\"
#endif
    f c    = T.singleton c

    compOpts = defaultCompOpt
      { multiline      = False
      , caseSensitive  = isLinux
      , lastStarGreedy = True
      }
    isLinux =
#ifdef mingw32_HOST_OS
      False
#else
      True
#endif

{-# INLINE reMatches #-}
reMatches :: CompiledRegex -> Text -> Bool
reMatches CompiledRegex{crRegex} = match crRegex
