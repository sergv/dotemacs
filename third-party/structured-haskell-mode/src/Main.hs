{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Take in Haskell code and output a vector of source spans and
-- their associated node type and case.

module Main (main) where

import Control.Applicative
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Descriptive
import Descriptive.Options
import System.Environment

import Data.ErrorMessage
import StructuredHaskellMode

-- | Action to perform.
data Action = Parse | Check

-- | Command line options.
options
  :: Monad m
  => Consumer [Text] (Option ()) m (Action, ParseType, Set Extension)
options = (,,) <$> action <*> typ <*> exts
  where
    action =
      constant "parse" "Parse and spit out spans" Parse <|>
      constant "check" "Just check the syntax" Check
    typ =
      constant "decl" "Parse a declaration" Decl <|>
      constant "stmt" "Parse a statement" Stmt
    exts =
      fmap (either (error . TL.unpack . emMessage) id . getExtensions)
           (many (prefix "X" "Language extension"))

sourceSpanToElisp :: SourceSpan -> String
sourceSpanToElisp SourceSpan{ssType, ssConstructor, ssStartLine, ssStartColumn, ssEndLine, ssEndColumn} =
  "[" ++ spanContent ++ "]"
  where
    spanContent = unwords
      [ T.unpack ssType
      , T.unpack ssConstructor
      , show ssStartLine
      , show ssStartColumn
      , show ssEndLine
      , show ssEndColumn
      ]

outputForElisp :: [SourceSpan] -> IO ()
outputForElisp spans =
  putStrLn $ "[" ++ concatMap sourceSpanToElisp spans ++ "]"

--- | Main entry point.
main :: IO ()
main = do
  code <- SourceCode <$> getContents
  args <- getArgs
  case consume options $ map T.pack args of
    Succeeded (action, typ, exts) -> do
      let res = case action of
            Parse -> outputForElisp <$> parseSpans typ exts code
            Check -> putStrLn "[]"  <$  check      typ exts code
      case res of
        Left err -> error $ parseErrorMessage err
        Right x  -> x
    _ ->
      error (T.unpack (textDescription (describe options [])))

