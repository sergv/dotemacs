----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Monday, 15 August 2016
-- The actual server that handles tags
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Haskell.Language.Server.Tags
  ( startTagsServer
  , stopTagsServer
  , waitForTagsServerFinish
  , TagsServer(tsRequestHandler)
  , RequestHandler
  , TagsServerConf(..)
  , defaultTagsServerConf
  , TagsServerState(..)
  , emptyTagsServerState
  ) where

import Control.Concurrent
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except.Ext
import Control.Monad.Trans.Control
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.Zlib as Zlib
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import Data.Semigroup.Foldable
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Store as Store
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Ext (Pretty(..), (##), (<+>))
import qualified System.Directory as Directory
import qualified System.FSNotify as FSNotify
import System.IO

import qualified Data.Promise as Promise

import Control.Monad.Filesystem (MonadFS(..), SearchCfg(..))
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
import Data.CompiledRegex
import Data.ErrorMessage
import Data.Filesystem
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Path
import Data.Symbols (fileNameToModuleName, resolvedSymbolFile)
import Haskell.Language.Server.Tags.LoadFiles
import Haskell.Language.Server.Tags.LoadModule
import Haskell.Language.Server.Tags.Search
import Haskell.Language.Server.Tags.SearchM
import Haskell.Language.Server.Tags.Types
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

data TagsServer = TagsServer
  { -- | The way to communicate with running tags server: send requests and
    -- get promises of responses.
    tsRequestHandler :: RequestHandler
    -- | Lock that becomes available when server exits.
  , tsFinishState    :: MVar TagsServerState
    -- | Id of the requent serving thread.
  , tsThreadId       :: !ThreadId
  , tsFileWatch      :: FSNotify.WatchManager
  }

stopTagsServer :: MonadBase IO m => TagsServer -> m ()
stopTagsServer TagsServer{tsThreadId, tsFileWatch} = liftBase $ do
  FSNotify.stopManager tsFileWatch
  killThread tsThreadId

-- | Block until tags server stops.
waitForTagsServerFinish :: MonadBase IO m => TagsServer -> m TagsServerState
waitForTagsServerFinish = liftBase . readMVar . tsFinishState

searchCfgIgnoredRE
  :: MonadError ErrorMessage m
  => SearchCfg
  -> m CompiledRegex
searchCfgIgnoredRE SearchCfg{scIgnoredGlobs} =
  fileGlobsToRegex (scIgnoredGlobs <> MonadFS.defaultIgnoredGlobs)

preloadFiles
  :: (WithCallStack,  MonadError ErrorMessage m, MonadLog m, MonadFS m)
  => SearchCfg
  -> TagsServerConf
  -> TagsServerState
  -> m TagsServerState
preloadFiles SearchCfg{scShallowPaths, scRecursivePaths} _ s
  | S.null scShallowPaths && S.null scRecursivePaths
  = pure s
preloadFiles searchCfg conf s = do
  ignoredGlobsRE <- searchCfgIgnoredRE searchCfg
  knownFiles     <- MonadFS.findRec searchCfg ignoredGlobsRE (loadMod conf) (const (pure Nothing))
  let s' = s
        { tssKnownFiles =
          M.fromList (concatMap (\(impKey, fs) -> map (,impKey) fs) $ M.toList $ toList . NEMap.keysNE <$> knownFiles) <>
          tssKnownFiles s
        }
  if tsconfEagerTagging conf
  then do
    logInfo "[preloadFiles] collecting tags eagerly..."
    s'' <- loadAllFilesIntoState (fold1 . NEMap.elemsNE <$> knownFiles) conf s'
    logInfo "[preloadFiles] collecting tags eagerly... OK"
    pure s''
  else pure s'
    { tssUnloadedFiles =
      M.unionWith (<>) (fold1 . NEMap.elemsNE <$> knownFiles) (tssUnloadedFiles s)
    }

watchDirs
  :: (MonadError ErrorMessage m, MonadBase IO m)
  => TagsServerConf
  -> FSNotify.WatchManager
  -> Chan SomeRequest
  -> SearchCfg
  -> m ()
watchDirs conf manager reqChan searchCfg@SearchCfg{scShallowPaths, scRecursivePaths, scIgnoredDirs} = do
  ignoredGlobsRE <- searchCfgIgnoredRE searchCfg
  let shouldAct :: FSNotify.Event -> Bool
      shouldAct event =
        not (FSNotify.eventIsDirectory event) &&
        case classifyPath conf path' of
          Nothing -> False
          -- Must check that path does not match ignored globs because they
          -- may include filename patterns as well as directory patterns.
          Just{}  -> not (reMatches ignoredGlobsRE path)
        where
          path' = mkSinglePathFragment path
          path = T.pack $ FSNotify.eventPath event
      reportEvent' :: FilePath -> (FullPath 'File -> FSNotifyEvent) -> IO ()
      reportEvent' path f = do
        path' <- runExceptT $ mkFullPath path
        case path' of
          Left _       -> pure ()
          Right path'' -> writeChan reqChan $ SomeRequest (FSNotifyReq $ f path'') ()
      reportEvent :: FSNotify.Event -> IO ()
      reportEvent = \case
        FSNotify.Added    path _ _ -> reportEvent' path FSAdded
        FSNotify.Modified path _ _ -> reportEvent' path FSModified
        FSNotify.Removed  path _ _ -> reportEvent' path FSRemoved
        FSNotify.Unknown{}         -> pure ()
  liftBase $ do
    (dirsToWatch :: Set (FullPath 'Dir)) <- findRecurCollect
      scIgnoredDirs          -- ignored dirs
      ignoredGlobsRE
      mempty                 -- shallow paths
      scRecursivePaths       -- recursive  dirs
      scShallowPaths         -- initial value
      (const (pure Nothing)) -- consume file
      (pure . Just)
    for_ dirsToWatch $ \dir ->
      FSNotify.watchDir manager (toFilePath dir) shouldAct reportEvent

-- | Start new tags server thread that will serve requests supplied via returned
-- RequestHandler.
startTagsServer
  :: forall m. (WithCallStack, MonadBase IO m, MonadBaseControl IO m, MonadCatch m, MonadError ErrorMessage m, MonadLog m, MonadFS m, MonadMask m)
  => SearchCfg
  -> TagsServerConf
  -> m TagsServer
startTagsServer searchCfg conf = do
  tsFileWatch   <- liftBase FSNotify.startManager
  initState     <- case tsconfSerialisedState conf of
    Nothing   -> pure emptyTagsServerState
    Just file -> do
      exists <- liftBase $ Directory.doesFileExist file
      if exists
      then do
        contents <- liftBase $ withFile file ReadMode $ \h ->
          C.runConduit $
            C.sourceHandleUnsafe h .| Zlib.decompress Zlib.defaultWindowBits .| C.sinkLbs
        case Store.decode $ BSL.toStrict contents of
          Left msg -> throwErrorWithCallStack $ "Failed to read server state from" <+> pretty file ## pretty (show msg)
          Right x  -> pure x
      else pure emptyTagsServerState
  initState'    <- preloadFiles searchCfg conf initState
  reqChan       <- liftBase newChan
  tsFinishState <- liftBase newEmptyMVar
  tsThreadId    <- liftBaseDiscard forkIO $ handleRequests tsFinishState reqChan tsFileWatch initState'
  watchDirs conf tsFileWatch reqChan searchCfg
  let tsRequestHandler :: RequestHandler
      tsRequestHandler = \case
        req@QueryReq{} -> do
          respPromise <- Promise.newPromise
          writeChan reqChan (SomeRequest (UserReq req) respPromise)
          pure respPromise
        req@FinishReq{} -> do
          respPromise <- Promise.newPromise
          writeChan reqChan (SomeRequest (UserReq req) respPromise)
          pure respPromise
  pure TagsServer
    { tsRequestHandler
    , tsFinishState
    , tsThreadId
    , tsFileWatch
    }
  where
    handleRequests
      :: MVar TagsServerState
      -> Chan SomeRequest
      -> FSNotify.WatchManager
      -> TagsServerState
      -> m ()
    handleRequests doneLock reqChan manager = go
      where
        go :: TagsServerState -> m ()
        go s = do
          req <- liftBase $ readChan reqChan
          s'  <- handleReq req s `onException` serialiseState s
          case s' of
            Nothing  -> pure ()
            Just s'' -> go s''
        serialiseState :: TagsServerState -> m ()
        serialiseState s = do
          case tsconfSerialisedState conf of
            Nothing   -> pure ()
            Just dest -> do
              logInfo $ "[startTagsServer.handleReq] storing state in" <+> pretty dest
              liftBase $ C.runConduitRes $
                C.sourceLbs (BSL.fromStrict (Store.encode s)) .| Zlib.compress 9 Zlib.defaultWindowBits .| C.sinkFileCautious dest
          liftBase $ putMVar doneLock s
        handleReq
          :: SomeRequest
          -> TagsServerState
          -> m (Maybe TagsServerState)
        handleReq req serverState = case req of
          SomeRequest (FSNotifyReq event) () -> do
            logInfo $ "[startTagsServer.handleReq] file notification event:" ## pretty event
            fmap Just $ case event of
              FSAdded path -> do
                mmods <- loadMod conf path
                pure $ case mmods of
                  Nothing             -> serverState
                  Just (impKey, mods) -> serverState
                    { tssUnloadedFiles =
                      M.insertWith (<>) impKey (fold1 $ NEMap.elemsNE mods) $ tssUnloadedFiles serverState
                    , tssKnownFiles    =
                      M.insert path impKey $ tssKnownFiles serverState
                    }
              FSRemoved path ->
                pure $ case M.updateLookupWithKey (\_ _ -> Nothing) path $ tssKnownFiles serverState of
                  (Nothing,     _)              -> serverState
                  (Just target, tssKnownFiles') -> serverState
                    { tssLoadedModules =
                      M.delete target $ tssLoadedModules serverState
                    , tssUnloadedFiles =
                      M.delete target $ tssUnloadedFiles serverState
                    , tssKnownFiles    = tssKnownFiles'
                    }
              FSModified path ->
                pure $ case M.lookup path $ tssKnownFiles serverState of
                  Nothing     -> serverState
                  Just impKey -> serverState
                    { tssLoadedModules =
                      M.adjust (fmap (\m -> m { modIsDirty = True })) impKey $ tssLoadedModules serverState
                    , tssUnloadedFiles =
                      M.adjust (fmap (\m -> m { modIsDirty = True })) impKey $ tssUnloadedFiles serverState
                    }
          SomeRequest (UserReq request) respPromise -> do
            -- (request, responsePromise) <- liftBase $ readChan reqChan
            logInfo $ "[startTagsServer.handleReq] request:" ## pretty request
            case request of
              FinishReq -> do
                serialiseState serverState
                Promise.putValue respPromise (Right ())
                pure Nothing
              QueryReq filename request' ns -> do
                let loadedNS   = tssNamespace serverState
                    searchCfg' = SearchCfg
                      { scShallowPaths   = nsShallowDirs   ns S.\\ nsShallowDirs loadedNS
                      , scRecursivePaths = nsRecursiveDirs ns S.\\ nsRecursiveDirs loadedNS
                      , scIgnoredDirs    = scIgnoredDirs searchCfg
                      , scIgnoredGlobs   = nsIgnoredGlobs  ns S.\\ nsIgnoredGlobs loadedNS
                      }
                serverState' <- preloadFiles searchCfg' conf serverState
                let serverState'' = serverState' { tssNamespace = tssNamespace serverState' <> ns }
                watchDirs conf manager reqChan searchCfg'
                (response, serverState''') <- runSearchT conf serverState'' $ do
                  symbols <- case request' of
                    FindSymbol scope symbol ->
                      findSymbol id scope filename symbol
                    FindSymbolByRegex scope regexp ->
                      findSymbolByRegexp id scope filename regexp
                  logInfo $ "[startTagsServer.handleReq] requested namespace:" ## pretty ns
                  pure $ case filter (isPathWithinNamespace ns . resolvedSymbolFile) $ toList symbols of
                    []   -> NotFound
                    s:ss -> Found $ s :| ss
                logInfo $ "[startTagsServer.handleReq] response:" ## either pretty pretty response
                Promise.putValue respPromise response
                pure $ Just serverState'''

-- todo: handle header files here
classifyPath :: TakeExtension a => TagsServerConf -> a -> Maybe ImportTarget
classifyPath TagsServerConf{tsconfVanillaExtensions, tsconfHsBootExtensions} path
  | ext `S.member` tsconfVanillaExtensions = Just VanillaModule
  | ext `S.member` tsconfHsBootExtensions  = Just HsBootModule
  | otherwise                              = Nothing
  where
    ext  = takeExtension path

loadMod
  :: (MonadFS m, MonadError ErrorMessage m, MonadLog m)
  => TagsServerConf
  -> FullPath 'File
  -> m (Maybe (ImportKey, NonEmptyMap (FullPath 'File) (NonEmpty UnresolvedModule)))
loadMod conf filename =
  case classifyPath conf filename of
    Nothing         -> pure Nothing
    Just importType -> do
      modTime       <- MonadFS.getModificationTime filename
      suggestedName <- fileNameToModuleName filename
      unresolvedMod@Module{modHeader = ModuleHeader{mhModName}} <-
        readFileAndLoad (Just suggestedName) modTime filename
      unresolvedMod `seq` pure (Just (ImportKey importType mhModName, NEMap.singleton filename (unresolvedMod :| [])))

