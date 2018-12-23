----------------------------------------------------------------------------
-- |
-- Module      :  ${haskell path to module name}
-- Copyright   :  (c) ${author} ${date year}
-- License     :  ${license type} (see LICENSE)
-- Maintainer  :  ${email}
----------------------------------------------------------------------------

{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module ${haskell path to module name} (main) where

import Options.Applicative

data Config = Config
  { cfgInputFile :: !FilePath
  , cfgSomeFlag  :: !Bool
  , cfgSomeEnum  :: Test
  , cfgSomeInt   :: !(Maybe Int)
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

  -- cfgCustomReader <- option (eitherReader readSeverity) $
  --   long "verbosity" <>
  --   value Error <>
  --   showDefaultWith showSeverity <>
  --   help ("Debug verbosity. Known values: " ++ L.intercalate ", " knownSeverities)

  pure Config{..}

progInfo :: ParserInfo Config
progInfo = info
  (helper <*> optsParser)
  (fullDesc <> header "My new shiny program!")

main :: IO ()
main = do
  Config{cfgInputFile, cfgSomeFlag, cfgSomeInt} <- execParser progInfo

  pure ()
