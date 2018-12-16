----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Server.Tags.LoadFiles
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 12 October 2016
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Server.Tags.LoadFiles
  ( loadAllFilesIntoState
  ) where

import Control.Arrow ((&&&))
import Control.Monad.Except.Ext
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Semigroup as Semigroup
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext

import Control.Monad.Logging
import Data.ErrorMessage
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Path
import Haskell.Language.Server.Tags.LoadModule (resolveModule)
import Haskell.Language.Server.Tags.Types
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

{-# INLINE loadAllFilesIntoState #-}
-- | A quick way to resolve all known modules, assuming there're no other
-- modules we don't know about.
--
-- Most of the speed comes from not touching the file system.
loadAllFilesIntoState
  :: forall m. (WithCallStack, MonadError ErrorMessage m, MonadLog m)
  => Map ImportKey (NonEmpty UnresolvedModule)
  -> TagsServerConf
  -> TagsServerState
  -> m TagsServerState
loadAllFilesIntoState unresolvedModules conf@TagsServerConf{tsconfNameResolution} initState = do
  let checkLoadingModules
        :: forall n. MonadState TagsServerState n
        => ImportKey
        -> n (Maybe (NonEmpty UnresolvedModule, [ResolvedModule]))
      checkLoadingModules key = do
        TagsServerState{tssLoadsInProgress, tssLoadedModules} <- get
        pure $ case M.lookup key tssLoadsInProgress of
          Just modules -> Just (NEMap.elemsNE modules, loadedMods)
            where
              loadedMods :: [ResolvedModule]
              loadedMods = foldMap toList $ M.lookup key tssLoadedModules
          Nothing      -> Nothing

      doResolve
        :: forall n. (WithCallStack, MonadState TagsServerState n, MonadError ErrorMessage n, MonadLog n)
        => ImportKey
        -> n (Maybe (NonEmpty ResolvedModule))
      doResolve key = do
        resolveState <- get
        case M.lookup key $ tssLoadedModules resolveState of
          Just resolved -> pure $ Just resolved
          Nothing       -> do
            logInfo $ "[loadAllFilesIntoState.doResolve] Resolving" <+> PP.dquotes (pretty (ikModuleName key))
            let currentlyLoading = tssLoadsInProgress resolveState
            if key `M.member` currentlyLoading
            then
              throwErrorWithCallStack $ PP.hsep
                [ "[loadAllFilesIntoState.doResolve] found import loop: module"
                , PP.dquotes (pretty key)
                , "was required while being loaded"
                ]
            else
              case M.lookup key unresolvedModules of
                Nothing         -> do
                  let msg = PP.hsep
                        [ "[loadAllFilesIntoState.doResolve] imported module"
                        , PP.dquotes (pretty key)
                        , "not found"
                        ]
                  case tsconfNameResolution of
                    NameResolutionLax -> do
                      logWarning msg
                      pure Nothing
                    NameResolutionStrict -> throwErrorWithCallStack msg
                Just unresolved -> do
                  let unresolvedMap :: NonEmptyMap (FullPath 'File) UnresolvedModule
                      unresolvedMap = NEMap.fromNonEmpty $ (modFile &&& id) <$> unresolved
                  logDebug $ "[loadAllFilesIntoState.doResolve] currently loading:" ## ppMapWith pretty (ppNE . NEMap.keysNE) currentlyLoading
                  modify $ \s ->
                    s { tssLoadsInProgress = M.insertWith NEMap.union key unresolvedMap $ tssLoadsInProgress s }
                  -- logDebug $ "[loadAllFilesIntoState.doResolve] files:" ## ppNE (modFile <$> unresolved)
                  resolved <- flip runReaderT conf $
                    traverse (resolveModule checkLoadingModules doResolve) unresolved
                  modify $ \s -> s
                    { tssLoadsInProgress =
                        M.update (`NEMap.difference` unresolvedMap) key $ tssLoadsInProgress s
                    , tssLoadedModules   =
                        M.insertWith (Semigroup.<>) key resolved $ tssLoadedModules s
                    }
                  logInfo $ "[loadAllFilesIntoState.doResolve] Resolved" <+> PP.dquotes (pretty (ikModuleName key))
                  pure $ Just resolved

  flip execStateT initState $
    flip M.traverseMaybeWithKey unresolvedModules $ \importKey _ ->
      doResolve importKey
