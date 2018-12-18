----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Server.Sexp
-- Copyright   :  (c) Sergey Vinokurov 2016
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD-2 (see LICENSE)
--
-- Sexp frontend for tag server.
----------------------------------------------------------------------------

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Haskell.Language.Server.Sexp
  ( sexpDefaultPort
  , SexpServer
  , stopSexpServer
  , waitForSexpServerStart
  , runSexpServer
  ) where

import Control.Concurrent
import Control.DeepSeq
import qualified Control.Exception as Exception
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.ErrorExcept
import Control.Monad.Except (throwError)
import Control.Monad.Except.Ext
import Control.Monad.Trans.Control

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific as Scientific
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Prettyprint.Doc.Ext
import qualified Network.Socket as Network

import Language.Sexp as Sexp
import Network.Socket as Socket
import qualified Network.Socket.ByteString.Lazy as Socket.BSL

import Control.Monad.Logging
import Data.CompiledRegex
import Data.Condition
import Data.ErrorMessage
import Data.Path
import qualified Data.Promise as Promise
import Data.Symbols
import Haskell.Language.Lexer.FastTags (Type, Line(..))
import Haskell.Language.Server.Tags.Types

sexpDefaultPort :: Network.PortNumber
sexpDefaultPort = 4872

data SexpServer = SexpServer
  { ssThreadId    :: ThreadId
  , ssStartedLock :: Condition
  }

stopSexpServer :: MonadBase IO m => SexpServer -> m ()
stopSexpServer = liftBase . killThread . ssThreadId

-- | After this function returns the server is guaranteed to be ready
-- to receive new connections.
waitForSexpServerStart :: MonadBase IO m => SexpServer -> m ()
waitForSexpServerStart = waitForCondition . ssStartedLock

withSocketLocalhostAccept
  :: Network.PortNumber
  -> Condition
  -> (Network.Socket -> IO ())
  -> IO ()
withSocketLocalhostAccept port startedLock serveRequest = withSocketsDo $ do
  let hints = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  localhost :_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))
  Exception.bracket
    (socket (addrFamily localhost) (addrSocketType localhost) (addrProtocol localhost))
    Socket.close $
    \sock -> do
      setSocketOption sock ReuseAddr 1
      bind sock $ addrAddress localhost
      listen sock maxListenQueue
      setCondition startedLock
      forever $ do
        (socket', _addr) <- Socket.accept sock
        forkFinally (serveRequest socket') (const (Socket.close socket'))

runSexpServer
  :: forall m. (WithCallStack, MonadBase IO m, MonadBaseControl IO m, MonadLog m, MonadCatch m, StM m Sexp ~ Sexp, StM m () ~ ())
  => Network.PortNumber
  -> RequestHandler
  -> m SexpServer
runSexpServer port reqHandler = do
  ssStartedLock <- newUnsetCondition
  ssThreadId    <- liftBaseWith $ \runInBase ->
    forkIO $
      withSocketLocalhostAccept port ssStartedLock $ \sock -> do
        contents <- Socket.BSL.getContents sock
        Exception.evaluate $ rnf contents
        runInBase $ do
          logVerboseDebug $ "[runSexpServer] got raw request:" ## ppByteStringLazy contents
          response <- case Sexp.decode contents of
            Left err -> pure $ ParenList
              [ Symbol "invalid-request"
              , String $ T.pack err
              , String $ TL.toStrict $ TLE.decodeUtf8With TE.lenientDecode contents
              ]
            Right req -> go req
          let response' = Sexp.encode response
          logVerboseDebug $ "[runSexpServer] sending raw response:" ## ppByteStringLazy response'
          liftBase $ Socket.BSL.sendAll sock response'
          liftBase $ Socket.shutdown sock Socket.ShutdownSend
  pure SexpServer
    { ssThreadId
    , ssStartedLock
    }
  where
    go :: WithCallStack => Sexp -> m Sexp
    go args = do
      logInfo $ "[runSexpServer.go] got request:" ## pretty args
      case args of
        ParenList [Symbol func, ParenList args'] -> do
          res <- runErrorExceptT $ go' func args'
          pure $ case res of
            Left err -> ParenList [Symbol "error", String err']
              where
                err' :: T.Text
                err' = TL.toStrict $ displayDoc $ pretty err
            Right x  -> ParenList [Symbol "ok", x]
        ParenList [Symbol func, Symbol "nil"] -> do
          res <- runErrorExceptT $ go' func []
          pure $ case res of
            Left err -> ParenList [Symbol "error", String err']
              where
                err' :: T.Text
                err' = TL.toStrict $ displayDoc $ pretty err
            Right x  -> ParenList [Symbol "ok", x]
        invalid -> pure $ ParenList
          [ Symbol "error"
          , String $ "Call argument must be of the form (function (arg0 arg1 ... argN)) or (function nil), but got: " <> T.pack (show invalid)
          ]
    go'
      :: WithCallStack
      => T.Text -> [Sexp] -> ErrorExceptT ErrorMessage m Sexp
    go' "finish" args =
      case args of
        [] -> do
          response <- liftBase $ Promise.getPromisedValue =<< reqHandler FinishReq
          either throwError (pure . (\() -> Symbol "nil")) response
        _ ->
          throwErrorWithCallStack $
            "Expected no arguments but got:" ## ppShow args
    go' "find" args =
      case args of
        [String filename, String symbol, scope, ns] -> do
          filename' <- mkFullPath filename
          scope'    <- decodeScope scope
          namespace <- decodeNamespace ns
          let request = QueryReq filename' (FindSymbol scope' (mkSymbolName symbol)) namespace
          response <- liftBase $ Promise.getPromisedValue =<< reqHandler request
          either throwError (pure . responseToTerm) response
        _ ->
          throwErrorWithCallStack $
            "Expected 4 arguments but got:" ## ppShow args
    go' "find-regex" args =
      case args of
        [String filename, String regexp, scope, ns] -> do
          filename' <- mkFullPath filename
          scope'    <- decodeScope scope
          request   <-
            QueryReq filename'
              <$> (FindSymbolByRegex scope' <$> compileRegex regexp)
              <*> decodeNamespace ns
          response <- liftBase $ Promise.getPromisedValue =<< reqHandler request
          either throwError (pure . responseToTerm) response
        _ ->
          throwErrorWithCallStack $
            "Expected 4 arguments but got:" ## ppShow args
    go' func _ =
      throwErrorWithCallStack $
        "Unexpected funciton:" <+> pretty func

decodeScope :: MonadError ErrorMessage m => Sexp -> m NameResolutionScope
decodeScope = \case
  Symbol "local"  -> pure ScopeCurrentModule
  Symbol "global" -> pure ScopeAllModules
  invalid           ->
    throwErrorWithCallStack $
      "Invalid scope:" ## ppShow invalid

decodeNamespace
  :: (MonadError ErrorMessage m, MonadBase IO m)
  => Sexp -> m Namespace
decodeNamespace = \case
  ParenList [shallowDirs, recursiveDirs, ignoredGlobs] -> do
    nsShallowDirs   <- fmap S.fromList . traverse (mkFullPath <=< decodeString) =<< decodeList shallowDirs
    nsRecursiveDirs <- fmap S.fromList . traverse (mkFullPath <=< decodeString) =<< decodeList recursiveDirs
    nsIgnoredGlobs  <- fmap S.fromList . traverse decodeString                  =<< decodeList ignoredGlobs
    pure Namespace{..}
  Symbol "nil" -> pure mempty
  invalid ->
    throwErrorWithCallStack $
      "Invalid namespace:" ## ppShow invalid

decodeString :: MonadError ErrorMessage m => Sexp -> m T.Text
decodeString = \case
  String str -> pure str
  invalid    ->
    throwErrorWithCallStack $
      "Invalid string:" ## ppShow invalid

decodeList :: MonadError ErrorMessage m => Sexp -> m [Sexp]
decodeList = \case
  Symbol "nil" -> pure []
  ParenList xs -> pure xs
  invalid      ->
    throwErrorWithCallStack $
      "Invalid list:" ## ppShow invalid


responseToTerm :: QueryResponse -> Sexp
responseToTerm = \case
  NotFound -> Symbol "not-found"
  Found (sym :| []) ->
    ParenList
      [ Symbol "loc-known"
      , symbolToSexp sym
      ]
  Found symbols ->
    ParenList
      [ Symbol "loc-ambiguous"
      , ParenList $ toList $ symbolToSexp <$> symbols
      ]

symbolToSexp :: ResolvedSymbol -> Sexp
symbolToSexp sym =
  ParenList
    [ String $ unqualSymNameText $ resolvedSymbolName sym
    , String $ unFullPath file
    , Number $ scientific (toInteger (unLine line)) 0
    , Symbol $ T.pack $ show typ
    ]
  where
    (file, line) = resolvedSymbolPosition sym
    typ :: Type
    typ = resolvedSymbolType sym
