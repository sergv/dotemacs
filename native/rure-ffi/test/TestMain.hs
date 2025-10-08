-- |
-- Module:     TestMain
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE OverloadedStrings #-}

module TestMain (main) where

import Data.ByteString.Char8 qualified as C8
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Data.Regex.Rure

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [regexTests, regexSetTests, allMatchesTests]

regexTests :: TestTree
regexTests = testGroup "Regex"
  [ testGroup ".*\\.c$" $
      [ mkTest $ TestInput (T.unpack str) ".*\\.c$" str result
      | (str, result) <-
        [ ("foo.c",   True)
        , ("foo.h",   False)
        , ("foo.txt", False)
        , ("foo.cc",  False)
        ]
      ]
  , testGroup ".*\\.h$" $
      [ mkTest $ TestInput (T.unpack str) ".*\\.h$" str result
      | (str, result) <-
        [ ("foo.c",   False)
        , ("foo.h",   True)
        , ("foo.txt", False)
        , ("foo.cc",  False)
        ]
      ]
  , testGroup ".*ивет$" $
      [ mkTest $ TestInput (T.unpack str) ".*ивет$" str result
      | (str, result) <-
        [ ("привет",  True)
        , ("приветы", False)
        ]
      ]
  , testGroup "〛" $
      [ mkTest $ TestInput (T.unpack str) "〛" str result
      | (str, result) <-
        [ ("〚decombobulate〛", True)
        ]
      ]
  ]

regexSetTests :: TestTree
regexSetTests = testGroup "RegexSet"
  [ testGroup ".*\\.c$, .*\\.h$" $
      [ mkSetTest $ SetTestInput (C8.unpack str) [".*\\.c$", ".*\\.h$"] str result
      | (str, result) <-
        [ ("foo.c",   True)
        , ("foo.h",   True)
        , ("foo.txt", False)
        , ("foo.cc",  False)
        ]
      ]
  ]

allMatchesTests :: TestTree
allMatchesTests = testGroup "All matches"
  [ mkAllMatchesTest $ TestAllMatchesInput
      { tamName   = "‘o’ against ‘foo’"
      , tamRegexp = "o"
      , tamStr    = "foo"
      , tamResult = [Match 1 2, Match 2 3]
      }
  , mkAllMatchesTest $ TestAllMatchesInput
      { tamName   = "‘x’ against ‘foo’"
      , tamRegexp = "x"
      , tamStr    = "foo"
      , tamResult = []
      }
  , mkAllMatchesTest $ TestAllMatchesInput
      { tamName   = "‘’ against ‘foo’"
      , tamRegexp = ""
      , tamStr    = "foo"
      , tamResult = map (\x -> Match x x) [0, 1, 2, 3]
      }
  , mkAllMatchesTest $ TestAllMatchesInput
      { tamName   = "‘.’ against ‘foo’"
      , tamRegexp = "."
      , tamStr    = "foo"
      , tamResult = map (\x -> Match x (x + 1)) [0, 1, 2]
      }
  ]

data TestInput = TestInput
  { tiName   :: !String
  , tiRegexp :: !Text
  , tiStr    :: !Text
  , tiResult :: !Bool
  }

mkTest :: TestInput -> TestTree
mkTest TestInput{tiName, tiRegexp, tiStr, tiResult} =
  testCase tiName $
    case compileRegex (T.encodeUtf8 tiRegexp) mempty Nothing of
      Left err -> assertFailure $
        "Failed to compile regexp set " ++ show tiRegexp ++ ": " ++ show err
      Right r  -> assertEqual
        ("Regexp " ++ show tiRegexp ++ " should " ++ (if tiResult then "match" else "not match") ++ " input ‘" ++ show tiStr ++ "’")
        tiResult
        (bytestringHasMatch r (T.encodeUtf8 tiStr))

data SetTestInput = SetTestInput
  { stiName    :: !String
  , stiRegexps :: ![C8.ByteString]
  , stiStr     :: !C8.ByteString
  , stiResult  :: !Bool
  }

mkSetTest :: SetTestInput -> TestTree
mkSetTest SetTestInput{stiName, stiRegexps, stiStr, stiResult} =
  testCase stiName $
    case compileRegexSet stiRegexps mempty Nothing of
      Left err -> assertFailure $
        "Failed to compile regexp set " ++ show stiRegexps ++ ": " ++ show err
      Right r  -> assertEqual
        ("Regexp set " ++ show stiRegexps ++ " should " ++ (if stiResult then "match" else "not match") ++ " input ‘" ++ show stiStr ++ "’")
        stiResult
        (bytestringHasSetMatch r stiStr)

data TestAllMatchesInput = TestAllMatchesInput
  { tamName   :: !String
  , tamRegexp :: !C8.ByteString
  , tamStr    :: !C8.ByteString
  , tamResult :: ![Match]
  }

mkAllMatchesTest :: TestAllMatchesInput -> TestTree
mkAllMatchesTest TestAllMatchesInput{tamName, tamRegexp, tamStr, tamResult} =
  testCase tamName $
    case compileRegex tamRegexp mempty Nothing of
      Left err -> assertFailure $
        "Failed to compile regexp set " ++ show tamRegexp ++ ": " ++ show err
      Right r  -> assertEqual
        ("Regexp " ++ show tamRegexp ++ " should have matches " ++ show (map prettyMatch tamResult) ++ " on string " ++ show tamStr)
        (map prettyMatch tamResult)
        (map prettyMatch (L.reverse (unReversedList (bytestringAllMatches r tamStr))))
  where
    prettyMatch :: Match -> (Int, Int)
    prettyMatch Match{matchStart, matchEnd} = (matchStart, matchEnd)
