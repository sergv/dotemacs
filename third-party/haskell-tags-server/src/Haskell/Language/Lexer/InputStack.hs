----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.InputStack
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :   2 June 2017
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Lexer.InputStack
  ( InputStack(..)
  , InputType(..)
  , take
  , uncons
  , lookupMacroArg
  ) where

import Control.Arrow (second)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

import Prelude hiding (take)

import Data.Symbols.MacroName (MacroName)

data InputType = OriginalSource | Macro

-- | Abstraction of input text into a stream so that it can have macro
-- definitions. When a macro is encountered, current stream is paused
-- and a new stream should be formed out of the macro definition.
data InputStack =
    OriginalSourceStack Text
  | ExpandingConstant
      !MacroName            -- ^ Macro name
      !Text                 -- ^ Macro body
      {-# UNPACK #-} !Int   -- ^ Length of macro text
      InputStack            -- ^ Rest of the stack
  | ExpandingFunction
      !MacroName            -- ^ Macro name
      !(Map MacroName Text) -- ^ Arguments
      !Text                 -- ^ Macro body
      {-# UNPACK #-} !Int   -- ^ Length of macro text
      InputStack            -- ^ Rest of the stack
  deriving (Eq, Ord, Show)

-- | Take prefix of length @n@ from input.
take :: Int -> InputStack -> Text
take = go []
  where
    go :: [Text] -> Int -> InputStack -> Text
    go acc !n = \case
      OriginalSourceStack txt ->
        T.concat $ T.take n txt : reverse acc
      ExpandingConstant _ txt k rest
        | n <= k    -> T.concat $ T.take n txt : reverse acc
        -- See [Space after constant macro end] for why we add " "
        | otherwise -> go (" " : txt : acc) (n - k - 1) rest
      ExpandingFunction _ _ txt k rest
        | n <= k    -> T.concat $ T.take n txt : reverse acc
        -- See [Space after function macro end] for why we add " "
        | otherwise -> go (" " : txt : acc) (n - k - 1) rest

uncons :: InputStack -> Maybe (InputType, Char, InputStack)
uncons = \case
  OriginalSourceStack txt                ->
    addInputType OriginalSource . second OriginalSourceStack <$> T.uncons txt
  ExpandingConstant _    _   0 rest      ->
    -- Note: [Space after constant macro end]
    -- When finished expanding a constant we must produce space so that it will
    -- not concatenate with what comes next. This is mainly needed due to the
    -- way we expand function macro applications - we match for "<name>( *"
    -- regexp and then check that <name> refers to a function macro. If it
    -- doesn't thet it still might refer to a macro definition so we should try
    -- expanding that but after we've done that it should be separated by a space
    -- from trailing data.
    Just (Macro, ' ', rest)
    -- uncons rest
  ExpandingConstant name txt n rest      ->
    addInputType Macro . second (\txt' -> ExpandingConstant name txt' (n - 1) rest) <$> T.uncons txt
  ExpandingFunction _    _    _   0 rest ->
    -- Note: [Space after function macro end]
    -- When macro function is finished we should produce a space to
    -- separate macro body from the rest of the program.
    --
    -- That is, this is needed to make following
    --
    -- #define foo() bar
    -- x = foo()baz
    --
    -- tokenise as
    -- [T "x", Equals, T "bar", T "baz"]
    Just (Macro, ' ', rest)
  ExpandingFunction name args txt n rest ->
    addInputType Macro . second (\txt' -> ExpandingFunction name args txt' (n - 1) rest) <$> T.uncons txt
  where
    addInputType :: t -> (a, b) -> (t, a, b)
    addInputType t (a, b) = (t, a, b)

-- | Check if @name@ refers to the macro function we're currently expanding.
lookupMacroArg :: MacroName -> InputStack -> Maybe Text
lookupMacroArg name = go
  where
    go = \case
      OriginalSourceStack{}          -> Nothing
      ExpandingConstant{}            -> Nothing
      ExpandingFunction _ args _ _ _ -> M.lookup name args

