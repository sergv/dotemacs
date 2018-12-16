----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.ErrorExcept
import Control.Monad.Except

import Data.Foldable (for_)
import qualified Data.List as L
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc.Ext
import Network.Socket (PortNumber, withSocketsDo)
import Options.Applicative
import System.Directory
import System.Exit
import System.IO

#ifndef mingw32_HOST_OS
import System.Posix (installHandler, sigPIPE, Handler(Ignore))
#endif

import Control.Monad.Filesystem (SearchCfg(..))
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
import Control.Monad.Logging.Simple
import Data.ErrorMessage
import Data.Path (mkFullPath)
import Haskell.Language.Server.Sexp
import Haskell.Language.Server.Tags
import Haskell.Language.Server.Tags.Types (NameResolutionStrictness(..))

data ProgramConfig = ProgramConfig
  { cfgSourceDirectories :: [FilePath] -- ^ Directories with haskell files to index
  , cfgDirTrees          :: [FilePath] -- ^ Directories that should be searched recursively - i.e.
                                       -- all their subdirectories are added to cfgSourceDirectories
  , cfgPort              :: PortNumber
    -- ^ Whether to eagerly read and resolve all tags at the start or to
    -- lazily load only required modules on a per-request basis.|
  , cfgEagerTagging      :: Bool
  , cfgNameResolution    :: NameResolutionStrictness
  , cfgDebugVerbosity    :: Severity
  , cfgStateFile         :: Maybe FilePath
  } deriving (Eq, Ord, Show)

optsParser :: Parser ProgramConfig
optsParser = ProgramConfig
  <$> many
        (strOption
           (long "dir" <>
            metavar "DIR" <>
            help "Add directory with haskell files to index"))
  <*> many
        (strOption
           (long "recursive" <>
            metavar "DIR" <>
            help "Recursively add directory tree with haskell files to index"))
  <*> option (fmap fromIntegral auto)
        (short 'p' <>
         long "port" <>
         value sexpDefaultPort <>
         metavar "PORT" <>
         help "Port to listen to")
  <*> switch
        (long "eager-tagging" <>
         help "Whether to load all tags at application start")
  <*> flag NameResolutionLax NameResolutionStrict
        (long "strict" <>
         help "Resolve names strictly forbidding any situations like exported name not being defined in a module")
  <*> option (eitherReader readSeverity)
        (long "verbosity" <>
         value Error <>
         showDefaultWith showSeverity <>
         help ("Debug verbosity. Known values: " ++ L.intercalate ", " knownSeverities))
  <*> optional
        (strOption
          (long "serialised-state" <>
           metavar "FILE" <>
           help "Store state between runs in this file"))

progInfo :: ParserInfo ProgramConfig
progInfo = info
  (helper <*> optsParser)
  (fullDesc <> header "Server for navigating Haskell programs")

main :: IO ()
main = withSocketsDo $ do

#ifndef mingw32_HOST_OS
  _ <- installHandler sigPIPE Ignore Nothing
#endif

  ProgramConfig{cfgSourceDirectories, cfgDirTrees, cfgPort, cfgEagerTagging, cfgNameResolution, cfgDebugVerbosity, cfgStateFile} <- execParser progInfo
  -- validate that specified directories actually exist
  for_ cfgSourceDirectories ensureDirExists
  for_ cfgDirTrees ensureDirExists

  runSimpleLoggerT (Just Stderr) cfgDebugVerbosity $ do
    tagsServer <- runErrorExceptT $ do
      cfgSourceDirectories' <- S.fromList <$> traverse mkFullPath cfgSourceDirectories
      cfgDirTrees'          <- S.fromList <$> traverse mkFullPath cfgDirTrees
      let searchDirs = SearchCfg
            { scShallowPaths   = cfgSourceDirectories'
            , scRecursivePaths = cfgDirTrees'
            , scIgnoredDirs    = MonadFS.versionControlDirs
            , scIgnoredGlobs   = MonadFS.defaultIgnoredGlobs
            }
          conf = defaultTagsServerConf
            { tsconfEagerTagging    = cfgEagerTagging
            , tsconfNameResolution  = cfgNameResolution
            , tsconfSerialisedState = cfgStateFile
            }
      logDebug $ ppDictHeader "Staring server with search dirs"
        [ "Search conf" --> searchDirs
        ]
      startTagsServer searchDirs conf :: ErrorExceptT ErrorMessage (SimpleLoggerT IO) TagsServer
    case tagsServer of
      Left err         -> liftIO $ putDocLn $ pretty err
      Right tagsServer' -> do
        sexpServer <- runSexpServer cfgPort $ tsRequestHandler tagsServer'
        waitForSexpServerStart sexpServer
        void $ waitForTagsServerFinish tagsServer'

ensureDirExists :: FilePath -> IO ()
ensureDirExists dir = do
  exists <- doesDirectoryExist dir
  unless exists $ do
    hPutStrLn stderr $ "error: directory " ++ dir ++ " does not exist"
    exitFailure
