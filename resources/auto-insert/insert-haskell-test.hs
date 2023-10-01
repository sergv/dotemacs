-- |
-- Module:     ${haskell path to module name}
-- Copyright:  (c) ${author} ${date year}
-- License:    ${license spdx} (see LICENSE)
-- Maintainer: ${email}

{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module ${haskell path to module name} (main) where

import Data.List qualified as L
import Options.Applicative
import System.Exit
import System.IO

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options qualified as Tasty
import Test.Tasty.QuickCheck qualified as QC
import Test.Tasty.Runners qualified as Tasty
import Test.Tasty.SmallCheck qualified as SC

-- import Data.Bimap (Bimap)
-- import Data.Bimap qualified as BM
--
-- data Verbosity = Debug | Info | Warning | Error | None
--   deriving (Eq, Ord, Show, Enum, Bounded)
--
-- knownVerbosities :: Bimap String Verbosity
-- knownVerbosities = BM.fromList
--   [ ("debug", Debug)
--   , ("info", Info)
--   , ("warning", Warning)
--   , ("error", Error)
--   , ("none", None)
--   ]
--
-- readVerbosity :: String -> Either String Verbosity
-- readVerbosity s = case BM.lookup s knownVerbosities of
--   Nothing -> Left $
--     "Invalid verbosity \"" ++ s ++ "\". Valid values: " ++ L.intercalate ", " (BM.keys knownVerbosities)
--   Just x  -> Right x
--
-- showVerbosity :: Verbosity -> String
-- showVerbosity v = case BM.lookupR v knownVerbosities of
--   Nothing -> error $ "Unknown verbosity " ++ show v
--   Just x  -> x

data Config = Config
  { cfgInputFile :: !FilePath
  , cfgSomeFlag  :: !Bool
  , cfgSomeEnum  :: Test
  , cfgSomeInt   :: !(Maybe Int)
  , cfgTastyOpts :: !Tasty.OptionSet
  -- , cfgVerbosity :: Verbosity
  }

-- suiteOptionParser

data Test = Test1 | Test2

optsParser :: Parser Tasty.OptionSet -> Parser Config
optsParser tastyParser = do
  cfgInputFile <- strOption $
    long "input" <>
    metavar "FILE" <>
    help "An input file"

  cfgSomeFlag <- switch $
    long "some-flag" <>
    help "A flag that does something"

  cfgSomeEnum <- flag Test1 Test2 $
    long "some-test-flag" <>
    help "Some other flag"

  cfgSomeInt   <- optional $ option (fmap fromIntegral auto) $
    short 'j' <>
    long "jobs" <>
    value 1 <>
    showDefault <>
    metavar "INT" <>
    help "Number of parallel jobs to run"

  cfgTastyOpts <- tastyParser

  -- cfgVerbosity <- option (eitherReader readVerbosity) $
  --   long "verbosity" <>
  --   value Error <>
  --   showDefaultWith showVerbosity <>
  --   completer (listCompleter (BM.keys knownVerbosities)) <>
  --   help ("Debug verbosity. Valid values: " ++ L.intercalate ", " (BM.keys knownVerbosities))

  pure Config{..}

progInfo :: Parser Tasty.OptionSet -> ParserInfo Config
progInfo tastyParser = info
  (helper <*> optsParser tastyParser)
  (fullDesc <> header "My new shiny test suite!")

main :: IO ()
main = do
  Tasty.installSignalHandlers

  let allTests    = tests
      ingredients = defaultIngredients
      tastyParser = snd $ Tasty.suiteOptionParser ingredients allTests

  Config{cfgInputFile, cfgSomeFlag, cfgSomeInt, cfgTastyOpts} <-
    customExecParser (prefs (showHelpOnEmpty <> noBacktrack <> multiSuffix "*")) (progInfo tastyParser)

  case Tasty.tryIngredients ingredients cfgTastyOpts allTests of
    Nothing  ->
      die "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
    Just act -> do
      ok <- act
      if ok then exitSuccess else exitFailure

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> L.sort (list :: [Int]) == L.sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , SC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> L.sort (list :: [Int]) == L.sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , localOption (QC.QuickCheckTests 10000) $
    QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]
