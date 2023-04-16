-- |
-- Module:     ${haskell path to module name}
-- Copyright:  (c) ${author} ${date year}
-- License:    ${license spdx} (see LICENSE)
-- Maintainer: ${email}

{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module ${haskell path to module name} (main) where

import Options.Applicative

-- import Data.Bimap (Bimap)
-- import Data.Bimap qualified as BM
-- import Data.List qualified as L
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
  -- , cfgVerbosity :: Verbosity
  }

data Test = Test1 | Test2

optsParser :: Parser Config
optsParser = do
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

  -- cfgVerbosity <- option (eitherReader readVerbosity) $
  --   long "verbosity" <>
  --   value Error <>
  --   showDefaultWith showVerbosity <>
  --   completer (listCompleter (BM.keys knownVerbosities)) <>
  --   help ("Debug verbosity. Valid values: " ++ L.intercalate ", " (BM.keys knownVerbosities))

  pure Config{..}

progInfo :: ParserInfo Config
progInfo = info
  (helper <*> optsParser)
  (fullDesc <> header "My new shiny program!")

main :: IO ()
main = do
  Config{cfgInputFile, cfgSomeFlag, cfgSomeInt} <-
    customExecParser (prefs (showHelpOnEmpty <> noBacktrack <> multiSuffix "*")) progInfo

  pure ()
