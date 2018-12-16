----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Main (main) where

import Network.Socket (withSocketsDo)
import System.IO
import Test.Tasty

#ifndef mingw32_HOST_OS
import System.Posix (installHandler, sigPIPE, Handler(Ignore))
#endif

import qualified Data.Map.NonEmpty.Tests as Data.Map.NonEmptyTests
import qualified Haskell.Language.Lexer.Tests as LexerTests
import qualified Haskell.Language.Lexer.Preprocessor.Tests as PreprocessorTests
import qualified Haskell.Language.Server.Tags.AnalyzeHeaderTests as AnalyzeHeaderTests
import qualified Haskell.Language.Server.Tags.TypesTests as TypesTests
import qualified ServerTests

main :: IO ()
main = withSocketsDo $ do

#ifndef mingw32_HOST_OS
  _ <- installHandler sigPIPE Ignore Nothing
#endif

  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  let tests = testGroup "Tests"
        [ Data.Map.NonEmptyTests.tests
        , ServerTests.tests
        , TypesTests.tests
        , AnalyzeHeaderTests.tests
        , LexerTests.tests
        , PreprocessorTests.tests
        ]
  defaultMainWithIngredients defaultIngredients tests
