----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.TypesTests
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Sunday, 18 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Server.Tags.TypesTests (tests) where

import Control.Arrow (second)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Data.Symbols

tests :: TestTree
tests = testGroup "Type tests"
  [ splitQualifiedPartTests
  ]

splitQualifiedPartTests :: TestTree
splitQualifiedPartTests = testGroup "splitting of qualified part of symbol"
  [ testGroup "unqualified"
      [ "foo" ==> (Nothing, mkSymbolName "foo")
      , "Typ" ==> (Nothing, mkSymbolName "Typ")
      , "++"  ==> (Nothing, mkSymbolName "++")
      , "."   ==> (Nothing, mkSymbolName ".")
      , ".+." ==> (Nothing, mkSymbolName ".+.")
      , "+.+" ==> (Nothing, mkSymbolName "+.+")
      , "+.+" ==> (Nothing, mkSymbolName "+.+")
      , "★"   ==> (Nothing, mkSymbolName "★")
      ]
  , testGroup "qualified"
      [ "Foo.Bar.foo" ==> (fooBarQual,         mkSymbolName "foo")
      , "Foo.Bar.Typ" ==> (fooBarQual,         mkSymbolName "Typ")
      , "Foo.Bar.++"  ==> (fooBarQual,         mkSymbolName "++")
      , "Foo.Bar.."   ==> (fooBarQual,         mkSymbolName ".")
      , "Foo.Bar..+." ==> (fooBarQual,         mkSymbolName ".+.")
      , "Foo.Bar.+.+" ==> (fooBarQual,         mkSymbolName "+.+")
      , "Foo.Bar.★"   ==> (fooBarQual,         mkSymbolName "★")
      , "Foo_.Bar_.★" ==> (mkQual "Foo_.Bar_", mkSymbolName "★")
      ]
  ]
  where
    (==>) = mkQualifierTest
    fooBarQual :: Maybe ImportQualifier
    fooBarQual = mkQual "Foo.Bar"
    mkQual = Just . mkImportQualifier . mkModuleName

mkQualifierTest
  :: Text
  -> (Maybe ImportQualifier, SymbolName)
  -> TestTree
mkQualifierTest input expected =
  testCase (T.unpack input) $ actual @?= expected
  where
    actual = second getUnqualifiedSymbolName (splitQualifiedPart (mkSymbolName input))
