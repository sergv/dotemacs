----------------------------------------------------------------------------
-- |
-- Module      :  MainServer
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  24 June 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TypeFamilies      #-}

module MainServer (main) where

import Control.Arrow (left)
import Control.Monad.Base
import Control.Monad.Except.Ext
import Control.Monad.Logger
import qualified Control.Monad.Logger.CallStack as Logger
import Control.Monad.Trans.Control
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Foldable
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Options.Applicative

import Data.BERT
import qualified Network.BERT.Server as BERT
import Network.Socket (PortNumber)

import Data.BERT.Pretty
import Data.Text.Prettyprint.Doc.Ext

import Data.ErrorMessage
import StructuredHaskellMode

data Config = Config
  { cfgPort     :: PortNumber
  , cfgLogLevel :: Maybe LogLevel
  }

optsParser :: Parser Config
optsParser = Config
  <$> option (fromInteger <$> auto)
        (long "port" <>
         value 8132 <>
         showDefault <>
         metavar "INT" <>
         help "Port to listen on")
  <*> optional
        (option (eitherReader readLogLevel)
          (long "log-level" <>
           metavar "LEVEL" <>
           help ("Logging severity. Possible values: " ++ L.intercalate ", " (fst <$> toList logLevels) ++ ".")))

readLogLevel :: String -> Either String LogLevel
readLogLevel = \input ->
  case M.lookup input levelsMap of
    Nothing -> Left $
      "Invalid log level: '" ++ input ++ "'. " ++
      "Expected: " ++ L.intercalate ", " (fst <$> toList logLevels) ++ "."
    Just x  -> Right x
  where
    levelsMap :: Map String LogLevel
    levelsMap = M.fromList $ toList logLevels

logLevels :: IsString a => NonEmpty (a, LogLevel)
logLevels =
    ("debug", LevelDebug) :|
  [ ("info",  LevelInfo)
  , ("warn",  LevelWarn)
  , ("error", LevelError)
  ]

progInfo :: ParserInfo Config
progInfo = info
  (helper <*> optsParser)
  (fullDesc <> header "Server for serving structured-haskell-mode for Emacs")

ppDispatchResult :: BERT.DispatchResult -> Doc a
ppDispatchResult = \case
  BERT.Success term        -> ppDict "Success" ["term" :-> ppBert term]
  BERT.NoSuchModule        -> "NoSuchModule"
  BERT.NoSuchFunction      -> "NoSuchFunction"
  BERT.Undesignated detail -> ppDict "Undesignated" ["detail" :-> pretty detail]

hseRequestHandler
  :: (HasCallStack, MonadBaseControl IO m, MonadLogger m)
  => String
  -> String
  -> [Term]
  -> m BERT.DispatchResult
hseRequestHandler mod func args = do
  Logger.logDebug $ TL.toStrict $ displayDoc $ ppDict "Handling request"
    [ "mod"  :-> pretty mod
    , "func" :-> pretty func
    , "args" :-> ppListDoc (map ppBert args)
    ]
  res <- runExceptT $ hseRequestHandler' mod func args
  let response = case res of
        Left err -> BERT.Success $ TupleTerm
          [ AtomTerm "arguments_error"
          , BinaryTerm $ TLE.encodeUtf8 $ formatErrorMessage $ unArgumentsError err
          ]
        Right x -> x
  Logger.logDebug $ TL.toStrict $ displayDoc $ ppDict "Response"
    [ "response" :-> ppDispatchResult response
    ]
  pure response

hseRequestHandler'
  :: (HasCallStack, MonadBaseControl IO m, MonadLogger m, MonadError ArgumentsError m)
  => String
  -> String
  -> [Term]
  -> m BERT.DispatchResult
hseRequestHandler' mod func args =
  case (mod, func) of
    ("structured-haskell-mode", "check") ->
      withArgs args check $ \callId () ->
        pure $ makeSuccessResult callId $ AtomTerm "ok"
    ("structured-haskell-mode", "parse") ->
      withArgs args parseSpans $ \callId spans -> do
        -- Logger.logInfo $ TL.toStrict $ displayDoc $ ppDict "Parsed spans"
        --   [ "spans" :-> ppList spans
        --   ]
        pure $ makeSuccessResult callId $ TupleTerm
          [ AtomTerm "spans"
          , spansToBertTerm spans
          ]
    ("structured-haskell-mode", _)       -> pure BERT.NoSuchFunction
    _                                    -> pure BERT.NoSuchModule

makeSuccessResult :: Term -> Term -> BERT.DispatchResult
makeSuccessResult callId resultData =
  BERT.Success $ TupleTerm [AtomTerm "success", callId, resultData]

newtype ArgumentsError = ArgumentsError { unArgumentsError :: ErrorMessage }

withArgs
  :: (HasCallStack, MonadError ArgumentsError m)
  => [Term]
  -> (ParseType -> Set Extension -> SourceCode -> Either StructuredHaskellMode.ParseError a)
  -> (Term -> a -> m BERT.DispatchResult)
  -> m BERT.DispatchResult
withArgs args f g =
  case args of
    [callId, parseType, exts, code] -> do
      parseType' <- extractParseType parseType
      exts'      <- extractExts exts
      code'      <- extractSourceCode code
      case f parseType' exts' code' of
        Left err -> pure $ makeSuccessResult callId $ TupleTerm
          [ AtomTerm "parse_error"
          , BinaryTerm $ TLE.encodeUtf8 $ TL.pack $ parseErrorMessage err
          , IntTerm $ parseErrorLine err
          , IntTerm $ parseErrorColumn err
          ]
        Right x  -> g callId x
    invalid -> throwErrorWithBacktrace' ArgumentsError $ TL.pack $
      "Expected 4 arguments, but got: " ++ show invalid

spansToBertTerm :: [SourceSpan] -> Term
spansToBertTerm = ListTerm . map convert
  where
    convert :: SourceSpan -> Term
    convert SourceSpan{ssType, ssConstructor, ssStartLine, ssStartColumn, ssEndLine, ssEndColumn} =
      TupleTerm
        [ BinaryTerm $ TLE.encodeUtf8 $ TL.fromStrict ssType
        , AtomTerm $ T.unpack ssConstructor
        , IntTerm ssStartLine
        , IntTerm ssStartColumn
        , IntTerm ssEndLine
        , IntTerm ssEndColumn
        ]

extractParseType
  :: (HasCallStack, MonadError ArgumentsError m) => Term -> m ParseType
extractParseType = \case
  AtomTerm "decl"  -> pure Decl
  AtomTerm "stmt"  -> pure Stmt
  AtomTerm invalid -> throwErrorWithBacktrace' ArgumentsError $ TL.pack $
    "Invalid parse type: '" <> invalid <> "'. Expected: 'decl', 'stmt'."
  invalid          -> throwErrorWithBacktrace' ArgumentsError $ TL.pack $
    "Invalid parse type: expected atom but got '" <> show invalid <> "'."

extractExts
  :: (HasCallStack, MonadError ArgumentsError m) => Term -> m (Set Extension)
extractExts = \case
  ListTerm xs
    | Just xs' <- traverse extractBinary xs ->
      either (throwError . ArgumentsError) pure $
      getExtensions =<< traverse (decodeUtf8 "extension") xs'
  invalid -> throwErrorWithBacktrace' ArgumentsError $
    "Invalid extensions: expected list of utf8 strings but got '" <> showLazy invalid <> "'"

extractSourceCode
  :: (HasCallStack, MonadError ArgumentsError m) => Term -> m SourceCode
extractSourceCode = \case
  BinaryTerm x -> either (throwError . ArgumentsError) pure $
    SourceCode . TL.unpack <$> (decodeUtf8Lazy "source code" x)
  invalid      -> throwErrorWithBacktrace' ArgumentsError $
    "Invalid extensions: expected list of utf8 strings but got '" <> showLazy invalid <> "'"

extractBinary :: Term -> Maybe C8.ByteString
extractBinary = \case
  BinaryTerm x -> Just x
  _            -> Nothing

decodeUtf8
  :: HasCallStack
  => TL.Text -> UTF8.ByteString -> Either ErrorMessage T.Text
decodeUtf8 thing =
  left (mkErrorMessage . mkErr) . TE.decodeUtf8' . C8.toStrict
  where
    mkErr :: Show a => a -> TL.Text
    mkErr x = "Invalid utf8 encoding of " <> thing <> ": " <> showLazy x

decodeUtf8Lazy
  :: HasCallStack
  => TL.Text -> UTF8.ByteString -> Either ErrorMessage TL.Text
decodeUtf8Lazy thing =
  left (mkErrorMessage . mkErr) . TLE.decodeUtf8'
  where
    mkErr :: Show a => a -> TL.Text
    mkErr x = "Invalid utf8 encoding of " <> thing <> ": " <> showLazy x

-- Start BERT server and serve from the current thread.
runServer
  :: (HasCallStack, MonadBaseControl IO m, MonadLogger m)
     -- Proof that m is nothing more than a reader, i.e. it carries no other state
  => (StM m BERT.DispatchResult ~ BERT.DispatchResult)
  => PortNumber
  -> m ()
runServer port = do
  tcp <- liftBase $ BERT.tcpServer port
  liftBaseWith $ \runInBase ->
    BERT.serve tcp (\x y z -> runInBase $ hseRequestHandler x y z)

main :: IO ()
main = do
  Config{cfgPort, cfgLogLevel} <- execParser progInfo
  case cfgLogLevel of
    Nothing    -> runNoLoggingT $ runServer cfgPort
    Just level ->
      runStderrLoggingT $
      filterLogger (\_ level' -> level' >= level) $
      runServer cfgPort
  return ()
