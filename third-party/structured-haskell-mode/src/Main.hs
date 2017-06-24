{-# LANGUAGE OverloadedStrings #-}

-- | Take in Haskell code and output a vector of source spans and
-- their associated node type and case.

module Main (main) where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Descriptive
import Descriptive.Options
import System.Environment

import StructuredHaskellMode

-- | Command line options.
options :: Monad m => Consumer [Text] (Option ()) m (Action, ParseType, [Extension])
options = (,,) <$> action <*> typ <*> exts
  where action =
          constant "parse" "Parse and spit out spans" Parse <|>
          constant "check" "Just check the syntax" Check
        typ =
          constant "decl" "Parse a declaration" Decl <|>
          constant "stmt" "Parse a statement" Stmt
        exts =
          fmap getExtensions
               (many (prefix "X" "Language extension"))

--- | Main entry point.
main :: IO ()
main =
  do code <- getContents
     args <- getArgs
     case consume options (map T.pack args) of
       Succeeded (action,typ,exts) ->
         outputWith action typ exts code
       _ ->
         error (T.unpack (textDescription (describe options [])))

