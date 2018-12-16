----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Tests
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday,  3 November 2016
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Haskell.Language.Lexer.Tests (tests) where

import Test.Tasty
import Test.Tasty.HUnit (testCase)

import Data.List (sort)
import qualified Data.Text as T

import Haskell.Language.Lexer (LiterateLocation(..))

import qualified Haskell.Language.Lexer.Tokenisation as Tokenisation
import Haskell.Language.Lexer.TokenisationUtils
import TestUtils (makeAssertion, makeTest)
import qualified Text.RawString.QQ as QQ

tests :: TestTree
tests = testGroup "Lexer tests"
  [ Tokenisation.tests
  , testTokenizeCpp
  , testFullPipeline
  ]

testTokenizeCpp :: TestTree
testTokenizeCpp = testGroup "Tokenize with preprocessor"
  [ testCase "Stripping of #define" $
    "#define FOO foo\n\
    \bar :: a -> a\n\
    \bar x = x"
    ==>
    [ Newline 0
    , Newline 0
    , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
    , T "bar", T "x", Equals, T "x", Newline 0
    ]
  , testCase "Stripping of multi-line #define #1" $
    "#define \\\n\
    \    FOO \\\n\
    \  foo\n\
    \bar :: a -> a\n\
    \bar x = x"
    ==>
    [ Newline 0
    , Newline 0
    , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
    , T "bar", T "x", Equals, T "x", Newline 0
    ]
  , testCase "Stripping of multi-line #define #2" $
    "#define FOO(T) \\\n\
    \{- hello there -} ;\\\n\
    \foo :: T -> T ;\\\n\
    \foo x = x\n\
    \bar :: a -> a\n\
    \bar x = x"
    ==>
    [ Newline 0
    , Newline 0
    , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
    , T "bar", T "x", Equals, T "x", Newline 0
    ]
  , testCase "Regression in 'text-show' package" $
    textShowSource
    ==>
    [ Newline 0
    , Newline 0
    , Newline 0
    , Newline 0
    , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
    , T "bar", T "x", Equals, T "x", Newline 0
    ]
  ]
  where
    (==>) = makeAssertion f
    f = map valOf . tokenize' filename Vanilla

_testTokenizeCppDefines :: TestTree
_testTokenizeCppDefines = testGroup "#define"
  [ constants
  , functions
  , concatenation
  , functionsAndConstants
  ]
  where
    (==>) = makeAssertion f
    f = map valOf . tokenize' filename Vanilla

    constants :: TestTree
    constants = testGroup "Constants"
      [ testCase "Vanilla define" $
          "#define FOO foo\n\
          \FOO :: a -> a\n\
          \FOO x = x"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Haskell-style define names with primes and backticks" $
          "#define FOO'Bar` foo\n\
          \FOO'Bar` :: a -> a\n\
          \FOO'Bar` x = x"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Define with lots of continuation lines and indentation" $
         "#  \\\n\
          \  define \\\n\
          \    FOO      \\\n\
          \       foo\n\
          \FOO :: a -> a\n\
          \FOO x = x"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Define with lots of continuation lines, no indentation and some spaces" $
          "#  \\\n\
          \define \\\n\
          \FOO      \\\n\
          \foo\n\
          \FOO :: a -> a\n\
          \FOO x = x"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Define with lots of continuation lines, no indentation and minimum spaces" $
          "#\\\n\
          \define \\\n\
          \FOO \\\n\
          \foo\n\
          \FOO :: a -> a\n\
          \FOO x = x"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Try to use constant define as a macro" $
          "#define FOO 1\n\
          \quux = FOO(2)\n"
          ==>
          [ Newline 0
          , Newline 0
          , T "quux", Equals, T "1", LParen, T "2", RParen, Newline 0
          ]
      , testCase "Define constant that spans boundaries of a string token" $
          "#define BAZ \"foo bar\n\
          \\n\
          \bar = BAZ FOO quux\"\n\
          \\n\
          \foo ::  a -> b"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, String, Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "b", Newline 0
          ]
      , testCase "Define multiline constant that spans boundaries of a string token" $
          "#define BAZ2 \"foo \\\n\
          \  bar\n\
          \\n\
          \bar = BAZ2 FOO quux\"\n\
          \\n\
          \foo ::  a -> b"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, String, Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "b", Newline 0
          ]
      , testCase "Stripping of empty c-style comments" $
          "#define TEST foo/**/bar\n\
          \\n\
          \concatTest :: a -> a\n\
          \concatTest x =\n\
          \  x + TEST + \"foobar\"   "
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "concatTest", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "concatTest", T "x", Equals, Newline 2
          , T "x", T "+", T "foobar", T "+", String, Newline 0
          ]
      , testCase "Stripping of non-empty c-style comments" $
          "#define TEST foo/* hello world! */bar\n\
          \\n\
          \concatTest :: a -> a\n\
          \concatTest x =\n\
          \  x + TEST + \"foobar\"   "
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "concatTest", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "concatTest", T "x", Equals, Newline 2
          , T "x", T "+", T "foobar", T "+", String, Newline 0
          ]
      , testCase "Define after use" $
          "foo = X\n\
          \\n\
          \#define X Y\n\
          \\n\
          \bar = X"
          ==>
          [ Newline 0
          , T "foo", Equals, T "X", Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, T "Y", Newline 0
          ]
      , testCase "Redefine" $
          "#define X Y\n\
          \\n\
          \foo = X\n\
          \\n\
          \#define X Z\n\
          \\n\
          \bar = X"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", Equals, T "Y", Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, T "Z", Newline 0
          ]
      , testCase "Expand vanilla multi-token #define" $
          "#define FOO a + b\n\
          \foo :: Int -> Int -> Int\n\
          \foo a b c = FOO + c * (FOO)\n"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "Int", Arrow, T "Int", Arrow, T "Int", Newline 0
          , T "foo", T "a", T "b", T "c", Equals, T "a", T "+", T "b", T "+", T "c", T "*", LParen, T "a", T "+", T "b", RParen, Newline 0
          ]
      , testCase "Expand #define that expands to another #define'd name" $
          "#define FOO foo\n\
          \#define BAR FOO\n\
          \\n\
          \BAR :: a -> b\n\
          \BAR x = x"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "b", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Expand #define that expands to name #define'd later in the program" $
          "#define BAR FOO\n\
          \#define FOO foo\n\
          \\n\
          \BAR :: a -> b\n\
          \BAR x = x"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "b", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      ]

    functions :: TestTree
    functions = testGroup "Functions"
      [ testCase "Function of 0 arguments without whitespace" $
          "#define foo() bar\n\
          \test :: a -> a\n\
          \test x = foo()baz"
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "bar", T "baz", Newline 0
          ]
      , testCase "Function of 0 arguments with whitespace in argument list at definition site" $
          "#define foo(           ) bar\n\
          \test :: a -> a\n\
          \test x = foo()baz"
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "bar", T "baz", Newline 0
          ]
      , testCase "Function of 0 arguments and whitespace in argument list at call site" $
          "#define foo() bar\n\
          \test :: a -> a\n\
          \test x = foo(           )baz"
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "bar", T "baz", Newline 0
          ]
      , testCase "Function of 0 arguments not equivalent to macro name" $
          "#define foo() bar\n\
          \test :: a -> a\n\
          \test x = foo baz"
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "foo", T "baz", Newline 0
          ]
      , testCase "Function of 1 argument" $
          "#define MKLENS(x) x\n\
          \MKLENS(bar) :: a -> a\n\
          \MKLENS(bar) x = x"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Function of 1 arguments not expanded instead of constant" $
          "#define foo() bar\n\
          \test :: a -> a\n\
          \test x = foo baz"
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "foo", T "baz", Newline 0
          ]
      , testCase "Function of 2 vanilla arguments" $
          "#define TEST(x, y) x + y\n\
          \bar :: a -> a\n\
          \bar x = TEST(x, x)"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", T "+",T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains comma within single quotes" $
          "#define TEST(x, y) x + y\n\
          \bar :: a -> a\n\
          \bar x = TEST(',', x)"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, Character, T "+",T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains comma within double quotes" $
          "#define TEST(x, y) x + y\n\
          \bar :: a -> a\n\
          \bar x = TEST(\"x, x\", x)"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, String, T "+",T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains comma within balanced parentheses" $
          "#define TEST(x, y) x + y\n\
          \bar :: a -> a\n\
          \bar x = TEST((x, x), x)"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, LParen, T "x", Comma, T "x", RParen, T "+",T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains balanced parentheses surrounded by other text" $
          "#define TEST(x, y) x + y\n\
          \bar :: a -> a\n\
          \bar x = TEST(y (x, x) * z, x)"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "y", LParen, T "x", Comma, T "x", RParen, T "*", T "z", T "+", T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains comma within many balanced parentheses" $
          "#define TEST(x, y) x + y\n\
          \bar :: a -> a\n\
          \bar x = TEST(((((((((((x, x)))))))))), x)"
          ==>
          ([ Newline 0
           , Newline 0
           , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
           , T "bar", T "x", Equals
           ] ++
           replicate 10 LParen ++ [T "x", Comma, T "x"] ++ replicate 10 RParen ++
           [T "+",T "x", Newline 0])
      , testCase "Function of 2 arguments - balanced brackets do not change semantics of comma" $
          "#define TEST(x, y) x + y\n\
          \bar :: a -> a\n\
          \bar x = TEST([x, x])"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, LBracket, T "x", T "+", T "x", RBracket, Newline 0
          ]
      , testCase "Function of 4 arguments with some arguments empty" $
          "#define TEST(x, y, z, w) x + y + z + w\n\
          \bar :: a -> a\n\
          \bar x = TEST(x * x,   ,, 2 )"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", T "*", T "x", T "+", T "+", T "+", T "2", Newline 0
          ]
      , testCase "Expand multiline function-like macro" $
          "#define TEST(name, tvar, var) \\\n\
          \  name :: tvar -> tvar \\\n\
          \  name var = var\n\
          \\n\
          \foo :: a -> a\n\
          \foo x = x\n\
          \TEST(bar, b, y)\n\
          \\n\
          \TEST(baz, c, z)"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          , T "bar", DoubleColon, T "b", Arrow, T "b"
          , T "bar", T "y", Equals, T "y", Newline 0
          , Newline 0
          , Newline 0
          , T "baz", DoubleColon, T "c", Arrow, T "c"
          , T "baz", T "z", Equals, T "z", Newline 0
          ]
      , testCase "Expand invalid function-like macro" $
          "#define FOO () foo()\n\
          \\n\
          \bar :: a -> ()\n\
          \bar = FOO() + 1"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, LParen, RParen, Newline 0
          , T "bar", Equals, LParen, RParen, T "foo", LParen, RParen, LParen, RParen, T "+", T "1", Newline 0
          ]
      , testCase "Stringization with # without spaces" $
          "#define TEST(x) x (#x)\n\
          \bar :: a -> a\n\
          \bar x = TEST(x)"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", LParen, String, RParen, Newline 0
          ]
      , testCase "Stringization with # surrounded with spaces" $
          "#define TEST(x) x (  #  x  )\n\
          \bar :: a -> a\n\
          \bar x = TEST(x)"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", LParen, String, RParen, Newline 0
          ]
      , testCase "Redefine" $
          "#define X(a) Y\n\
          \\n\
          \foo = X(1)\n\
          \\n\
          \#define X(a) Z\n\
          \\n\
          \bar = X(1)"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", Equals, T "Y", Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, T "Z", Newline 0
          ]
      , testCase "Function macro passed as argument to different function macro and applied there" $
          "#define BAR(x) 1\n\
          \#define BAZ(x) 2\n\
          \\n\
          \#define FOO(BAR, X) BAR(X)\n\
          \\n\
          \foo :: a -> a\n\
          \foo = FOO(BAZ, 3)"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", Equals, T "2", Newline 0
          ]
      ]

    concatenation :: TestTree
    concatenation = testGroup "Concatenation"
      [ -- Must do this because gcc and cpphs do this.
        -- Clang doesn't do this right, but that would be definitely
        -- nonportable, so don't aim for Clang.
        testCase "Via c-style comments" $
          "#define CONCAT_TEST(name) name/**/Test\n\
          \\n\
          \concatTest :: a -> a\n\
          \concatTest x =\n\
          \  x + CONCAT_TEST(bar) + \"foobar\"   "
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "concatTest", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "concatTest", T "x", Equals, Newline 2
          , T "x", T "+", T "barTest", T "+", String, Newline 0
          ]
        -- Gcc does not support this, but cpphs does and it's pretty standard
        -- in the C world.
      , testCase "Via ##" $
          "#define CONCAT_TEST(name) name##Test\n\
          \\n\
          \concatTest :: a -> a\n\
          \concatTest x =\n\
          \  x + CONCAT_TEST(bar) + \"foobar\"   "
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "concatTest", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "concatTest", T "x", Equals, Newline 2
          , T "x", T "+", T "barTest", T "+", String, Newline 0
          ]
      , testCase "Via ## with spaces" $
          "#define CONCAT_TEST(name) name ## Test\n\
          \\n\
          \concatTest :: a -> a\n\
          \concatTest x =\n\
          \  x + CONCAT_TEST(bar) + \"foobar\"   "
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "concatTest", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "concatTest", T "x", Equals, Newline 2
          , T "x", T "+", T "barTest", T "+", String, Newline 0
          ]
      ]

    functionsAndConstants :: TestTree
    functionsAndConstants = testGroup "Functions and constants"
      [ testCase "Function redefines constant" $
          "#define X Y\n\
          \\n\
          \foo = X\n\
          \\n\
          \#define X(a) Z\n\
          \\n\
          \bar = X(1)"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", Equals, T "Y", Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, T "Z", Newline 0
          ]
      , testCase "Function argument takes precedence over other defined macros" $
          "#define FOO(BAR) BAR\n\
          \\n\
          \#define BAR 15\n\
          \\n\
          \baz :: a -> a\n\
          \baz x = FOO(x) + 1"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "baz", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "baz", T "X", Equals, T "x", T "+", T "1", Newline 0
          ]
      , testCase "Function macro passed as argument to another function macro but not aplied there" $
          "#define BAR(x) 1\n\
          \#define BAZ(x) 2\n\
          \\n\
          \#define FOO(BAR, X) BAR\n\
          \\n\
          \foo :: a -> a\n\
          \foo = FOO(BAZ, 3)"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", Equals, T "BAZ", Newline 0
          ]
      , testCase "Constant macro passed as argument to another function macro gets expanded" $
          "#define BAR(x) 1\n\
          \#define BAZ 2\n\
          \\n\
          \#define FOO(BAR, X) BAR\n\
          \\n\
          \foo :: a -> a\n\
          \foo = FOO(BAZ, 3)"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", Equals, T "2", Newline 0
          ]
      ]

_testTokenizeCppConditionals :: TestTree
_testTokenizeCppConditionals = testGroup "Conditionals"
  [ testCase "Expand #ifdef-#endif" $
      "#ifdef FOO\n\
      \foo :: a -> a\n\
      \foo x = x\n\
      \#endif\n\
      \bar :: b -> b\n\
      \bar y = y\n\
      \\n\
      \\n"
      ==>
      [ Newline 0
      , Newline 0
      , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
      , T "foo", T "x", Equals, T "x", Newline 0
      , Newline 0
      , T "bar", DoubleColon, T "b", Arrow, T "b", Newline 0
      , T "bar", T "y", Equals, T "y", Newline 0
      , Newline 0
      , Newline 0
      ]
  , testCase "Expand both branches of #ifdef-#else-#endif" $
      "#ifdef FOO\n\
      \foo :: a -> a\n\
      \foo x = x\n\
      \#else\n\
      \bar :: b -> b\n\
      \bar y = y\n\
      \#endif\n"
      ==>
      [ Newline 0
      , Newline 0
      , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
      , T "foo", T "x", Equals, T "x", Newline 0
      , Newline 0
      , T "bar", DoubleColon, T "b", Arrow, T "b", Newline 0
      , T "bar", T "y", Equals, T "y", Newline 0
      , Newline 0
      ]
  , testCase "Expand all branches in #if-#elif-#else-#endif" $
      "#if defined(FOO)\n\
      \foo :: a -> a\n\
      \foo x = x\n\
      \#elif defined(BAR)\n\
      \bar :: b -> b\n\
      \bar y = y\n\
      \#else\n\
      \baz :: c -> c\n\
      \baz z = z\n\
      \#endif\n"
      ==>
      [ Newline 0
      , Newline 0
      , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
      , T "foo", T "x", Equals, T "x", Newline 0
      , Newline 0
      , T "bar", DoubleColon, T "b", Arrow, T "b", Newline 0
      , T "bar", T "y", Equals, T "y", Newline 0
      , Newline 0
      , T "baz", DoubleColon, T "c", Arrow, T "c", Newline 0
      , T "baz", T "z", Equals, T "z", Newline 0
      , Newline 0
      ]
  ]
  where
    (==>) = makeAssertion f
    f = map valOf . tokenize' filename Vanilla

_testTokenizeCppDefinesWithinConditionals :: TestTree
_testTokenizeCppDefinesWithinConditionals =
  testGroup "Defines within conditionals"
    [ testCase "Define same constant within conditional branches" $
        "#if defined(FOO)\n\
        \#define BAR x\n\
        \#else\n\
        \#define BAR y\n\
        \#endif\n\
        \\n\
        \BAR :: a -> b"
        ==>
        [ Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , T "x", DoubleColon, T "a", Arrow, T "b", Newline 0
        , T "y", DoubleColon, T "a", Arrow, T "b", Newline 0
        ]
    , testCase "Define same function within conditional branches" $
        "#if defined(FOO)\n\
        \#define BAR(a) x\n\
        \#else\n\
        \#define BAR(a) y\n\
        \#endif\n\
        \\n\
        \BAR(1) :: a -> b"
        ==>
        [ Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , T "x", DoubleColon, T "a", Arrow, T "b", Newline 0
        , T "y", DoubleColon, T "a", Arrow, T "b", Newline 0
        ]
    , testCase "Define constant and function with the same name within conditional branches" $
        "#if defined(FOO)\n\
        \#define BAR x\n\
        \#else\n\
        \#define BAR(a) y\n\
        \#endif\n\
        \\n\
        \BAR :: a -> b\n\
        \BAR(1) :: a -> b"
        ==>
        [ Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , T "x", DoubleColon, T "a", Arrow, T "b", Newline 0
        , T "y", DoubleColon, T "a", Arrow, T "b", Newline 0
        ]
    ]
  where
    (==>) = makeAssertion f
    f = map valOf . tokenize' filename Vanilla

testFullPipeline :: TestTree
testFullPipeline = testGroup "Full processing pipeline"
  [ ["data X", "module X"]
    ==>
    [ Pos (SrcPos "fn0" (Line 1) 0 mempty mempty) (TagVal "X" Type Nothing)
    , Pos (SrcPos "fn1" (Line 1) 0 mempty mempty) (TagVal "X" Module Nothing)
    ]
  -- Type goes ahead of Module.
  , [ "module X\n\
       \data X"
    ]
    ==>
    [ Pos (SrcPos "fn0" (Line 1) 0 mempty mempty) (TagVal "X" Module Nothing)
    , Pos (SrcPos "fn0" (Line 2) 0 mempty mempty) (TagVal "X" Type Nothing)
    ]
  , [ "module Z\n\
      \data X = Y\n"
    ]
    ==>
    [ Pos (SrcPos "fn0" (Line 1) 0 mempty mempty) (TagVal "Z" Module Nothing)
    , Pos (SrcPos "fn0" (Line 2) 0 mempty mempty) (TagVal "X" Type Nothing)
    , Pos (SrcPos "fn0" (Line 2) 0 mempty mempty) (TagVal "Y" Constructor (Just "X"))
    ]
  , [ "module Z\n\
      \data X a =\n\
      \  Y a\n"
    ]
    ==>
    [ Pos (SrcPos "fn0" (Line 1) 0 mempty mempty) (TagVal "Z" Module Nothing)
    , Pos (SrcPos "fn0" (Line 2) 0 mempty mempty) (TagVal "X" Type Nothing)
    , Pos (SrcPos "fn0" (Line 3) 0 mempty mempty) (TagVal "Y" Constructor (Just "X"))
    ]
  , [ "newtype A f a b = A\n\
      \  { unA :: f (a -> b) }"
    ]
    ==>
    [ Pos (SrcPos "fn0" (Line 1) 0 mempty mempty) (TagVal "A" Type Nothing)
    , Pos (SrcPos "fn0" (Line 1) 0 mempty mempty) (TagVal "A" Constructor (Just "A"))
    , Pos (SrcPos "fn0" (Line 2) 0 mempty mempty) (TagVal "unA" Function (Just "A"))
    ]
  ]
  where
    (==>) = makeTest f'
    f' = sort
       . concatMap (\(i, t) -> fst $ processTokens $ tokenize' ("fn" ++ show i) Vanilla t)
       . zip [0..]

textShowSource :: T.Text
textShowSource =
    [QQ.r|
#define GTEXT_SHOW(text_type,show_funs,no_show_funs,show1_funs,one_hash,two_hash,hash_prec,gtext_show,gshow_prec,gtext_show_con,gshow_prec_con,show_prec,lift_show_prec,show_space,show_paren,show_list_with,from_char,from_string) \
{- | A 'show_funs' value either stores nothing (for 'TextShow') or it stores            \
the two function arguments that show occurrences of the type parameter (for             \
'TextShow1').                                                                           \
                                                                                        \
/Since: 3.4/                                                                            \
-};                                                                                     \
data show_funs arity a where {                                                          \
    no_show_funs :: show_funs Zero a                                                    \
  ; show1_funs   :: (Int -> a -> text_type) -> ([a] -> text_type) -> show_funs One a    \
 } deriving Typeable;                                                                   \
                                                                                        \
instance Contravariant (show_funs arity) where {                                        \
    contramap _ no_show_funs       = no_show_funs                                       \
  ; contramap f (show1_funs sp sl) = show1_funs (\p -> sp p . f) (sl . map f)

bar :: a -> a
bar x = x|]
