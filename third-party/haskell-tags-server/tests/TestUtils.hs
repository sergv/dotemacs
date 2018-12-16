----------------------------------------------------------------------------
-- |
-- Module      :  TestUtils
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday, 13 October 2016
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module TestUtils
  ( TestCase(..)
  , mkUnqualSymName
  , neSingleton
  , makeTest
  , makeAssertion
  , makeAssertion'
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Ext
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, testCase, assertBool, assertFailure)

import Data.ErrorMessage
import Data.Symbols

data TestCase i o = TestCase
  { testName       :: String
  , input          :: i
  , expectedResult :: o
  } deriving (Eq, Ord, Show)

mkUnqualSymName :: T.Text -> UnqualifiedSymbolName
mkUnqualSymName name =
  fromMaybe err $ mkUnqualifiedSymbolName $ mkSymbolName name
  where
    err = error $ "invalid unqualified symbol name: " ++ show name

neSingleton :: a -> NonEmpty a
neSingleton x = x :| []

makeTest :: (Show a, Eq b, Show b) => (a -> b) -> a -> b -> TestTree
makeTest f x expected =
  testCase (take 70 $ show x) $ makeAssertion f x expected

makeAssertion :: (Show a, Eq b, Show b) => (a -> b) -> a -> b -> Assertion
makeAssertion f x expected = assertBool msg (actual == expected)
  where
    actual = f x
    msg    = "expected: " ++ show expected ++ "\n but got: " ++ show actual

makeAssertion'
  :: (Show a, Eq b, Show b)
  => (a -> Either ErrorMessage b) -> a -> b -> Assertion
makeAssertion' f x expected =
  case f x of
    Left msg     -> assertFailure $ displayDocString $
      "Got error message:" ## pretty msg
    Right actual -> assertBool msg (actual == expected)
      where
        msg = concat
          [ "expected: ", show expected, "\n"
          , " but got: ", show actual
          ]
