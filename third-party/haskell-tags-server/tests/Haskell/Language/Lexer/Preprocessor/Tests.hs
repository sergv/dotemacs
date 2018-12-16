----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Preprocessor.Tests
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  29 May 2017
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Lexer.Preprocessor.Tests (tests) where

import Test.Tasty
import Test.Tasty.HUnit (testCase)

import Control.Arrow (left)
import Data.Text (Text)
import qualified Data.Text.Prettyprint.Doc.Ext as PP

import Data.ErrorMessage
import Data.Symbols.MacroName (mkMacroName)
import Haskell.Language.Lexer.Preprocessor
import TestUtils


tests :: TestTree
tests = testGroup "Preprocessor parsing tests"
  [ defineTests
  , undefTests
  ]

mkConstantMacroDef :: Text -> Text -> ConstantMacroDef
mkConstantMacroDef name body = ConstantMacroDef
  { cmdName = mkMacroName name
  , cmdBody = body
  }

mkFunctionMacroDef :: Text -> [Text] -> Text -> FunctionMacroDef
mkFunctionMacroDef name args body = FunctionMacroDef
  { fmdName = mkMacroName name
  , fmdArgs = map mkMacroName args
  , fmdBody = body
  }

defineTests :: TestTree
defineTests = testGroup "#define"
  [ testGroup "Constants"
      [ testCase "Vanilla define" $
          "#define foo bar"
          ==> PreprocessorConstant (mkConstantMacroDef "foo" "bar")
      , testCase "Define with some spaces" $
          "# define FOO qu ux"
          ==> PreprocessorConstant (mkConstantMacroDef "FOO" "qu ux")
      , testCase "Define with lots of continuation lines" $
          "# \\\n\
          \ define \\\n\
          \ FOO \\\n\
          \ qu \\\n\
          \ ux"
          ==> PreprocessorConstant (mkConstantMacroDef "FOO" "qu  ux")
      , testCase "Define with name split by continuation line" $
          "#define FO\\\n\
          \O bar"
          ==> PreprocessorConstant (mkConstantMacroDef "FOO" "bar")
      , testCase "Malformed define starting with a number" $
          "#define 1foo bar"
          !=> Left "defined name > first cpp identifier char: Failed reading: satisfy"
      , testGroup "Haskell-style names"
          [ testCase "primes and backticks in the body" $
              "#define foo'_bar` baz"
              ==> PreprocessorConstant (mkConstantMacroDef "foo'_bar`" "baz")
          , testCase "prime at the start" $
              "#define 'foo_bar baz"
              ==> PreprocessorConstant (mkConstantMacroDef "'foo_bar" "baz")
          , testCase "backtick at the start" $
              "#define `foo_bar baz"
              ==> PreprocessorConstant (mkConstantMacroDef "`foo_bar" "baz")
          ]
      ]
  , testGroup "Macro functions"
      [ testGroup "Single argument"
          [ testCase "Vanilla define" $
              "#define FOO(x) x"
              ==> PreprocessorFunction (mkFunctionMacroDef "FOO" ["x"] "x")
          , testCase "Define with some spaces" $
              "# define FOO( x ) x"
              ==> PreprocessorFunction (mkFunctionMacroDef "FOO" ["x"] "x")
          ]
      , testGroup "Two arguments"
          [ testCase "Vanilla define" $
              "#define FOO(x, y) (x < y)"
              ==> PreprocessorFunction (mkFunctionMacroDef "FOO" ["x", "y"] "(x < y)")
          , testCase "Define with with some spaces" $
              "# define FOO( x , y ) ( x < y )"
              ==> PreprocessorFunction (mkFunctionMacroDef "FOO" ["x", "y"] "( x < y )")
          , testCase "Define with lots of continuation lines" $
              "# \\\n\
              \ define \\\n\
              \ FOO( \\\n\
              \ x \\\n\
              \ , \\\n\
              \ y \\\n\
              \ ) \\\n\
              \ ( x < \\\n\
              \ y )"
              ==> PreprocessorFunction (mkFunctionMacroDef "FOO" ["x", "y"] "( x <  y )")
      , testCase "Define with name split by continuation line" $
          "#define FO\\\n\
          \O(x \\\n\
          \ , \\\n\
          \ y) \\\n\
          \                                (y -\\\n\
          \x)"
          ==> PreprocessorFunction (mkFunctionMacroDef "FOO" ["x", "y"] "(y -x)")
          ]
      ]
  ]
  where
    (==>) = makeAssertion' parsePreprocessorDefine
    (!=>) = makeAssertion (left (PP.displayDocString . errorMessageBody) . parsePreprocessorDefine)


undefTests :: TestTree
undefTests = testGroup "#undef"
  [ testCase "Vanilla undef" $
      "#undef FOO"
      ==> mkMacroName "FOO"
  , testCase "Undef with some spaces" $
      "#   undef    FOO"
      ==> mkMacroName "FOO"
  , testCase "Undef with lots of continuation lines" $
      "# \\\n\
      \  undef  \\\n\
      \  F\\\n\
      \OO"
      ==> mkMacroName "FOO"
  ]
  where
    (==>) = makeAssertion' parsePreprocessorUndef
