----------------------------------------------------------------------------
-- |
-- Module      :  ServerTests
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServerTests (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Internal (ByteString(..), defaultChunkSize)
import qualified Network.Socket.ByteString as Socket.BS

import Control.Concurrent
import Control.DeepSeq
import Control.Exception (throwIO, ErrorCall(..), evaluate)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.ErrorExcept
import Control.Monad.Except
import Control.Monad.Trans.Control

import Data.Scientific
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext
import Data.Void (Void)
import GHC.Stack
import Language.Sexp as Sexp
import Network.Socket (PortNumber, Socket)
import Network.Socket as Socket
import qualified Network.Socket.ByteString.Lazy as Socket.BSL
import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Filesystem
import Data.ErrorMessage
import Data.Path
import Data.Symbols
import Haskell.Language.Server.Sexp
import Haskell.Language.Server.Tags
import Haskell.Language.Server.Tags.Types (NameResolutionStrictness(..))
import PortPool

import ServerTests.Data
import ServerTests.LogCollectingServer

-- | Directory with test projects.
testDataDir :: PathFragment
testDataDir = "test-data"

mkTestsConfig
  :: (MonadBase IO m, MonadError ErrorMessage m)
  => NameResolutionStrictness
  -> WorkingDirectory
  -> m (SearchCfg, TagsServerConf)
mkTestsConfig tsconfNameResolution srcDir = do
  searchDirs <- case srcDir of
    ShallowDir   dir -> do
      dir' <- mkFullPath $ testDataDir </> dir
      pure mempty { scShallowPaths = S.singleton dir' }
    RecursiveDir dir -> do
      dir' <- mkFullPath $ testDataDir </> dir
      pure mempty { scRecursivePaths = S.singleton dir' }
    RecursiveWithIgnored dir globs -> do
      dir' <- mkFullPath $ testDataDir </> dir
      pure mempty
        { scRecursivePaths = S.singleton dir'
        , scIgnoredGlobs   = S.fromList globs
        }
  let conf = defaultTagsServerConf
        { tsconfEagerTagging   = False
        , tsconfNameResolution
        }
  pure (searchDirs, conf)

tests :: TestTree
tests =
  withResource
    (getNumCapabilities >>= \caps -> newPortPool (2 * caps) (sexpDefaultPort + 1))
    (const (pure ()))
    (\pool -> makeTestTree pool testData)
  where
    makeTestTree :: IO PortPool -> TestSet ServerTest -> TestTree
    makeTestTree pool = go
      where
        go (GroupTest name xs)     = testGroup name $ map go xs
        go (AtomicTest serverTest) = mkFindSymbolTest pool serverTest

responseToSexp :: ServerResponse -> Sexp
responseToSexp resp =
  case resp of
    Known symName filename line typ ->
      ParenList [Symbol "loc-known", mkSymbol (symName, filename, line, typ)]
    Ambiguous xs ->
      ParenList [Symbol "loc-ambiguous", ParenList (map mkSymbol xs)]
    NotFound ->
      Symbol "not-found"
  where
    mkSymbol :: (UnqualifiedSymbolName, PathFragment, Integer, SymbolType) -> Sexp
    mkSymbol (symName, filename, line, typ) = ParenList
      [ String $ unqualSymNameText symName
      , String $ unPathFragment filename
      , Number $ scientific line 0
      , Symbol typ
      ]

data ServerConnection = ServerConnection
  { scServer :: LogCollectingServer
  , scSocket :: Socket
  }

reportErr :: MonadBase IO m => Doc Void -> m a
reportErr = liftBase . throwIO . ErrorCall . displayDocString

withConnection
  :: forall m a. (MonadMask m, MonadBaseControl IO m, MonadFS m)
  => IO PortPool
  -> SearchCfg
  -> TagsServerConf
  -> (ServerConnection -> m a)
  -> m a
withConnection pool searchDirs conf f =
  startLocalServer
  where
    startLocalServer :: m a
    startLocalServer = do
      pool' <- liftBase pool
      withPort pool' $ \port -> do
        serv <- runErrorExceptT $ mkLogCollectingServer searchDirs conf port
        case serv of
          Left err -> reportErr $
            "Failed to start local server:" ## pretty err
          Right serv' -> do
            waitUntilStart serv'
            (`finally` stopLogCollectingServer serv') $
              bracket
                (liftBase $ connectSocket "127.0.0.1" port)
                (liftBase . Socket.close)
                $ \scSocket ->
                f ServerConnection
                  { scServer = serv'
                  , scSocket
                  }

connectSocket :: String -> PortNumber -> IO Socket
connectSocket addr port = do
  let hints = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  localhost :_ <- getAddrInfo (Just hints) (Just addr) (Just (show port))
  s            <- socket (addrFamily localhost) (addrSocketType localhost) (addrProtocol localhost)
  Socket.connect s $ addrAddress localhost
  pure s

getSocketContents
  :: HasCallStack
  => Socket         -- ^ Connected socket
  -> IO BSL.ByteString  -- ^ Data received
getSocketContents sock = loop
  where
    loop = do
      s <- Socket.BS.recv sock (fromIntegral defaultChunkSize)
      if BS.null s
        then pure Empty -- <* Socket.shutdown sock Socket.ShutdownReceive
        else Chunk s <$> loop

mkFindSymbolTest
  :: HasCallStack
  => IO PortPool
  -> ServerTest
  -> TestTree
mkFindSymbolTest pool ServerTest{stTestName, stNameResolutionStrictness, stWorkingDirectory, stFile, stSymbol, stExpectedResponse} =
  testCase stTestName $ do
    result <- runErrorExceptT $ do
      (searchDirs, conf) <- mkTestsConfig stNameResolutionStrictness stWorkingDirectory
      withConnection pool searchDirs conf $ \conn -> do
        let dir :: PathFragment
            dir = case stWorkingDirectory of
              ShallowDir   x           -> testDataDir </> x
              RecursiveDir x           -> testDataDir </> x
              RecursiveWithIgnored x _ -> testDataDir </> x
            ns =
              case stWorkingDirectory of
                ShallowDir   _            -> ParenList [ParenList [String dir'], ParenList [], ParenList []]
                RecursiveDir _            -> ParenList [ParenList [], ParenList [String dir'], ParenList []]
                RecursiveWithIgnored _ xs -> ParenList [ParenList [], ParenList [String dir'], ParenList (map String xs)]
              where
                dir' = unPathFragment dir
            path = unPathFragment $ dir </> stFile
        r <- liftBase $ do
          let s = scSocket conn
          Socket.BSL.sendAll s $ Sexp.encode $
            ParenList [Symbol "find", ParenList [String path, String stSymbol, Symbol "local", ns]]
          Socket.shutdown s Socket.ShutdownSend
          r' <- Sexp.decode <$> getSocketContents s
          evaluate $ rnf r'
          pure r'
        logs <- do
          xs <- liftBase $ getLogs $ scServer conn
          pure $ "Logs:" ## PP.indent 2 (PP.vcat xs)
        case r of
          Left err  -> assertFailure' $ ppShow err ## logs
          Right res -> case res of
            ParenList [Symbol "error", err] -> assertFailure' $
              "Unexpected error from tags server:" <+> ppShow err
            ParenList [Symbol "ok", res'] -> do
              dir'   <- mkFullPath dir
              actual <- relativizePathsInResponse dir' res'
              let msg = docFromString $ "expected: " ++ show expected ++ "\n but got: " ++ show actual
              if responseType actual == responseType expected
              then
                unless (actual == expected) $
                  assertFailure' $ msg ## logs
              else
                assertFailure' $
                  case extractResponseError actual of
                    Nothing   -> msg ## logs
                    Just msg' -> "Error from server:" ##
                      PP.nest 2 (docFromText msg') ## logs
            unexpected -> assertFailure' $
              "Unexpected response from tags server:" <+> ppShow unexpected
            where
              expected = responseToSexp stExpectedResponse
    case result of
      Right () -> pure ()
      Left err -> assertFailure' $ "Failure:" ## pretty err
  where
    assertFailure' :: MonadBase IO m => Doc ann -> m a
    assertFailure' = liftBase . assertFailure . displayDocString

responseType :: Sexp -> Maybe Text
responseType (ParenList (Symbol x : _)) = Just x
responseType _                          = Nothing

extractResponseError :: Sexp -> Maybe Text
extractResponseError (ParenList [Symbol "error", String msg]) = Just msg
extractResponseError _                                        = Nothing

relativizePathsInResponse
  :: forall m. (MonadBase IO m, MonadError ErrorMessage m)
  => FullPath 'Dir -> Sexp -> m Sexp
relativizePathsInResponse rootDir term =
  case term of
    ParenList [a@(Symbol "loc-known"), loc] -> do
      loc' <- fixLoc loc
      pure $ ParenList [a, loc']
    ParenList [a@(Symbol "loc-ambiguous"), ParenList locs] -> do
      locs' <- traverse fixLoc locs
      pure $ ParenList [a, ParenList locs']
    x -> pure x
  where
    fixLoc :: Sexp -> m Sexp
    fixLoc (ParenList [symName, String path, line, typ]) = do
      path' <- relativise path
      pure $ ParenList [symName, String path', line, typ]
    fixLoc x = error $ "invalid symbol location term: " ++ show x
    relativise :: Text -> m Text
    relativise = fmap (either mkRel mkRel) . mkSomeFullPath
    mkRel
      :: MakeRelative (FullPath 'Dir) (FullPath a) PathFragment
      => FullPath a -> Text
    mkRel = unPathFragment . makeRelative rootDir

-- _baseNameToUTF8 :: BaseName -> UTF8.ByteString
-- _baseNameToUTF8 = pathFragmentToUTF8 . unBaseName

-- _fullPathToUTF8 :: FullPath -> UTF8.ByteString
-- _fullPathToUTF8 = CL8.fromStrict . TE.encodeUtf8 . unFullPath

-- fullPathFromUTF8
--   :: (MonadBase IO m, MonadError ErrorMessage m)
--   => UTF8.ByteString
--   -> m (Either (FullPath 'File) (FullPath 'Dir))
-- fullPathFromUTF8 = mkSomeFullPath . TE.decodeUtf8 . CL8.toStrict
--
-- pathFragmentToUTF8 :: PathFragment -> UTF8.ByteString
-- pathFragmentToUTF8 = CL8.fromStrict . TE.encodeUtf8 . unPathFragment
