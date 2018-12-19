----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.LoadModule
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Server.Tags.LoadModule
  ( loadModule
  , loadModule'
  , readFileAndLoad
  , loadModuleFromSource
  , resolveModule
  , checkLoadingModules
  , visibleNamesFromImportSpec
  ) where

import Prelude hiding (mod)

import Control.Arrow (first)
import qualified Control.Monad.Except as CME
import Control.Monad.Except.Ext
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (MonadWriter(..))
import qualified Control.Monad.Writer as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import Control.Parallel.Strategies.Ext

import qualified Data.ByteString as BS
import Data.Either
import Data.Foldable.Ext
import Data.Functor.Product (Product(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe hiding (Maybe(Just))
import qualified Data.Monoid as Monoid
import Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import Data.Void (Void)

import Haskell.Language.Lexer (tokenize)
import Haskell.Language.Lexer.FastTags (Pos, ServerToken, processTokens)
import qualified Haskell.Language.Lexer.FastTags as FastTags

import Control.Monad.Filesystem (MonadFS)
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
import Data.ErrorMessage
import Data.KeyMap (KeyMap)
import qualified Data.KeyMap as KM
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.MonoidalMap (MonoidalMap)
import qualified Data.MonoidalMap as MM
import Data.Path
import Data.SubkeyMap (SubkeyMap)
import qualified Data.SubkeyMap as SubkeyMap
import Data.SymbolMap (SymbolMap)
import qualified Data.SymbolMap as SM
import Data.Symbols
import Haskell.Language.Server.Tags.AnalyzeHeader
import Haskell.Language.Server.Tags.Types
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

defaultModuleName :: ModuleName
defaultModuleName = mkModuleName "Main"

loadModule'
  :: forall m n. (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => (MonadFS n, MonadError ErrorMessage n, MonadLog n)
  => (forall a. n a -> m a)
  -> ImportKey
  -> m [ResolvedModule]
loadModule' liftN key = do
  TagsServerConf{tsconfNameResolution} <- ask
  mods <- loadModule liftN key
  case (tsconfNameResolution, mods) of
    (NameResolutionStrict, Nothing)    -> throwErrorWithCallStack $
      "Failed to resolve import" <+> PP.squotes (pretty key)
    (NameResolutionLax,    Nothing)    -> pure mempty
    (_,                    Just mods') -> pure $ toList mods'

-- | Fetch module by it's name from cache or load it. Check modification time
-- of module files and reload if anything changed
loadModule
  :: forall m n. (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => (MonadFS n, MonadError ErrorMessage n, MonadLog n)
  => (forall a. n a -> m a)
  -> ImportKey
  -> m (Maybe (NonEmpty ResolvedModule))
loadModule liftN key@ImportKey{ikModuleName} = do
  logInfo $ "[loadModule] loading" <+> pretty ikModuleName
  s <- get
  if key `M.member` tssLoadsInProgress s
  then
    throwErrorWithCallStack $ PP.hsep
      [ "Import cycle detected: import of"
      , PP.dquotes (pretty key)
      , "is already in progress."
      , ppFoldableHeaderWith ppNEMap "All imports in progress:" $ tssLoadsInProgress s
      ]
  else do
    mods <- case M.lookup key (tssLoadedModules s) of
      Nothing -> do
        mods' <- doLoad
        case mods' of
          []     -> pure Nothing
          m : ms -> do
            let mods'' = m :| ms
            Just mods'' <$ modify (\s' -> s' { tssLoadedModules = M.insert key mods'' $ tssLoadedModules s' })
      Just ms -> do
        logDebug $ "[loadModule] module was loaded before, reusing:" <+> pretty key
        (ms', Any anyReloaded) <- fmap (first catMaybes) $ Strict.runWriterT $ for (toList ms) $ \m -> do
          m' <- lift $ reloadIfNecessary liftN key m
          case m' of
            Gone            -> pure Nothing
            AlreadyUpToDate -> pure $ Just m
            Reloaded m''    -> Just m'' <$ tell (Any True)
        case ms' of
          []       -> pure Nothing
          m : ms'' -> do
            let ms''' = m :| ms''
            when anyReloaded $
              modify $ \s' -> s' { tssLoadedModules = M.insert key ms''' $ tssLoadedModules s' }
            pure $ Just ms'''
    -- for_ mods $ \mods' ->
    --   logDebug $ ppFoldableHeader "[loadModule] loaded modules:" mods'
    pure mods
  where
    doLoad :: WithCallStack => m [ResolvedModule]
    doLoad = do
      logDebug $ "[loadModule.doLoad] module was not loaded before, loading now:" <+> pretty ikModuleName
      TagsServerState{tssUnloadedFiles} <- get
      case M.updateLookupWithKey (\_ _ -> Nothing) key tssUnloadedFiles of
        (Nothing, _) -> do
          let msg = "Cannot load module " <> pretty ikModuleName Semigroup.<> ": no paths found"
          TagsServerConf{tsconfNameResolution} <- ask
          case tsconfNameResolution of
            NameResolutionStrict -> throwErrorWithCallStack msg
            NameResolutionLax    -> [] <$ logWarning msg
        (Just mods, tssUnloadedFiles') -> do
          modify $ \s -> s { tssUnloadedFiles = tssUnloadedFiles' }
          fmap catMaybes $ for (toList mods) $ \m -> do
            m' <- reloadIfNecessary liftN key m
            case m' of
              Gone            -> pure Nothing
              AlreadyUpToDate -> Just <$> registerAndResolve liftN key m
              Reloaded m''    -> pure $ Just m''

data ReloadResult a =
    Reloaded a
  | AlreadyUpToDate
  | Gone
  deriving (Eq, Ord, Show)

-- TODO: consider using hashes to track whether a module needs reloading?
reloadIfNecessary
  :: (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => (MonadFS n, MonadError ErrorMessage n, MonadLog n)
  => (forall a. n a -> m a)
  -> ImportKey
  -> Module b
  -> m (ReloadResult ResolvedModule)
reloadIfNecessary liftN key@ImportKey{ikModuleName} m@Module{modFile, modHeader} = do
  exists <- MonadFS.doesFileExist modFile
  if exists
  then do
    (needsReloading, modifTime) <- moduleNeedsReloading m
    if needsReloading
    then do
      logInfo $ "[reloadIfNecessary] reloading module" <+> pretty (mhModName modHeader)
      m' <- registerAndResolve liftN key =<< readFileAndLoad (Just ikModuleName) modifTime modFile
      pure $ Reloaded m'
    else pure AlreadyUpToDate
  else pure Gone

registerAndResolve
  :: (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => (MonadFS n, MonadError ErrorMessage n, MonadLog n)
  => (forall a. n a -> m a)
  -> ImportKey
  -> UnresolvedModule
  -> m ResolvedModule
registerAndResolve liftN key unresolvedMod@Module{modFile} = do
  modify $ \s -> s
    { tssLoadsInProgress =
      M.insertWith NEMap.union key (NEMap.singleton modFile unresolvedMod) $ tssLoadsInProgress s
    }
  resolved <- resolveModule checkLoadingModules (loadModule liftN) unresolvedMod
  modify $ \s -> s
    { tssLoadsInProgress = M.update f key $ tssLoadsInProgress s }
  pure resolved
  where
    f :: NonEmptyMap (FullPath 'File) v -> Maybe (NonEmptyMap (FullPath 'File) v)
    f = NEMap.delete modFile

readFileAndLoad
  :: (MonadFS m, MonadError ErrorMessage m, MonadLog m)
  => Maybe ModuleName
  -> UTCTime
  -> FullPath 'File
  -> m UnresolvedModule
readFileAndLoad suggestedModName modTime filename = do
  source <- MonadFS.readFile filename
  logInfo $ "[readFileAndLoad] Loading" <+> PP.dquotes (pretty filename)
  loadModuleFromSource suggestedModName modTime filename source

checkLoadingModules
  :: forall m. MonadState TagsServerState m
  => ImportKey
  -> m (Maybe (NonEmpty UnresolvedModule, [ResolvedModule]))
checkLoadingModules key = do
  TagsServerState{tssLoadsInProgress, tssLoadedModules} <- get
  pure $ case M.lookup key tssLoadsInProgress of
    Just modules -> Just (NEMap.elemsNE modules, loadedMods)
      where
        loadedMods :: [ResolvedModule]
        loadedMods = foldMap toList $ M.lookup key tssLoadedModules
    Nothing      -> Nothing

-- | Load single module from the given file. Does not load any imports or exports.
-- Names in the loaded module are not resolved.
loadModuleFromSource
  :: (WithCallStack, MonadError ErrorMessage m, MonadLog m)
  => Maybe ModuleName
  -> UTCTime
  -> FullPath 'File
  -> BS.ByteString
  -> m UnresolvedModule
loadModuleFromSource suggestedModuleName modifTime filename source =
  makeModule suggestedModuleName modifTime filename tokens
  where
    tokens =
      tokenize (T.unpack $ unFullPath filename) source

makeModule
  :: (WithCallStack, MonadError ErrorMessage m, MonadLog m)
  => Maybe ModuleName -- ^ Suggested module name, will be used if source does not define it's own name.
  -> UTCTime
  -> FullPath 'File
  -> [Pos ServerToken]
  -> m UnresolvedModule
makeModule suggestedModuleName modifTime filename tokens = do
  (header, tokens') <- analyzeHeader filename tokens
  let syms           :: [ResolvedSymbol]
      errors         :: [Doc Void]
      (syms, errors) = first (fmap (mkResolvedSymbol filename) . FastTags.removeDuplicatePatterns)
                     $ processTokens tokens'
      allSymbols     :: SymbolMap
      allSymbols     = SM.fromList syms
  unless (null errors) $
    logError $ ppFoldableHeaderWith id
      ("fast-tags errors while loading" <+> pretty filename)
      errors
  -- case (suggestedModuleName, header) of
  --   (Just name, Just ModuleHeader{mhModName})
  --     | name == mkModuleName "Setup" || mhModName == mkModuleName "Main"
  --     -> pure ()
  --     | otherwise
  --     ->
  --       unless (name == mhModName) $
  --         throwErrorWithCallStack $ ppDictHeader "Module name within file differs from the expected module name"
  --           [ "file"                 --> filename
  --           , "module name in file"  --> mhModName
  --           , "expected module name" --> name
  --           ]
  --   _ -> pure ()
  let moduleHeader :: ModuleHeader
      moduleHeader = fromMaybe defaultHeader header
      mod :: UnresolvedModule
      mod = Module
        { modHeader           = moduleHeader
        , modAllSymbols       = allSymbols
        , modFile             = filename
        , modLastModified     = modifTime
        , modAllExportedNames = ()
        , modIsDirty          = False
        }
  -- logVerboseDebug $ "[makeModule] created module" <+> pretty mod
  pure mod
  where
    defaultHeader :: ModuleHeader
    defaultHeader = ModuleHeader
      { mhModName          = fromMaybe defaultModuleName suggestedModuleName
      , mhImports          = mempty
      , mhImportQualifiers = mempty
      , mhExports          = NoExports
      }

resolveModule
  :: forall m. (WithCallStack, MonadError ErrorMessage m, MonadReader TagsServerConf m, MonadLog m)
  => (ImportKey -> m (Maybe (NonEmpty UnresolvedModule, [ResolvedModule])))
  -> (ImportKey -> m (Maybe (NonEmpty ResolvedModule)))
  -> UnresolvedModule
  -> m ResolvedModule
resolveModule checkIfModuleIsAlreadyBeingLoaded readAndLoad mod = do
  logDebug $ "[resolveModule] resolving names of module" <+> pretty (mhModName header)
  symbols <- resolveExportedNames mod
  logVerboseDebug $ ppDictHeader ("[resolveModule] Resolved items for module" <+> pretty (mhModName header))
    [ -- "imports"                          :-> ppSubkeyMapWith pretty pretty ppNE imports
    -- ,
      "all symbols exported by a module" --> symbols
    ]
  pure $ mod { modAllExportedNames = symbols }
  where
    header :: ModuleHeader
    header = modHeader mod
    unresFile :: FullPath 'File
    unresFile = modFile mod

    resolveImports
      :: WithCallStack
      => SubkeyMap ImportKey (NonEmpty ImportSpec)
      -> m ( SubkeyMap ImportKey (NonEmpty (ImportSpec, SymbolMap))
           , [(ImportQualification, ModuleName, SymbolMap)]
           )
    resolveImports imports = do
      logDebug $
        "[resolveModule.resolveImports] analysing imports of module" <+> pretty (mhModName header) <+>
        "from" <+> pretty unresFile
      Lazy.runWriterT (SubkeyMap.traverseMaybeWithKey resolveImport imports)
      where
        resolveImport
          :: WithCallStack
          => ImportKey
          -> NonEmpty ImportSpec
          -> Lazy.WriterT [(ImportQualification, ModuleName, SymbolMap)] m (Maybe (NonEmpty (ImportSpec, SymbolMap)))
        resolveImport key importSpecs = do
          isBeingLoaded <- lift $ checkIfModuleIsAlreadyBeingLoaded key
          case isBeingLoaded of
            -- Standard code path: imported modules are already loaded and
            -- resolved, use what was resolved.
            Nothing -> do
              logDebug $ "[resolveModule.resolveImports.resolveImport] Resolving import" <+> pretty (ikModuleName key)
              modules <- lift $ readAndLoad key
              case modules of
                Nothing       -> do
                  -- The import is not found. Record that it brings no names
                  -- under relevant qualifiers so that later on those names
                  -- could be defaulted as coming from the export list.
                  tell $ flip map (toList importSpecs) $
                    \ImportSpec{ispecImportKey = ImportKey{ikModuleName = importedModName}, ispecQualification} ->
                      (ispecQualification, importedModName, mempty)
                  pure Nothing
                Just modules' -> do
                  -- This import was found to refer to modules within modules'.
                  let importedNames :: SymbolMap
                      !importedNames = foldMap modAllExportedNames modules'
                  TagsServerConf{tsconfNameResolution} <- ask
                  let results :: NonEmpty (Either ErrorMessage (ImportSpec, SymbolMap))
                      results = runEval $ parTraversable rseq $ flip fmap importSpecs $
                        \spec@ImportSpec{ispecImportKey = ImportKey{ikModuleName = importedModName}} ->
                          case visibleNamesFromImportSpec tsconfNameResolution importedModName importedNames spec of
                            Left err -> Left err
                            Right x  -> Right (spec, x)
                  results' <- for results $ \case
                    Left err -> CME.throwError err
                    Right (spec@ImportSpec{ispecImportKey = ImportKey{ikModuleName}, ispecQualification}, importedNames') -> do
                      -- Record which names enter current module's scope under certain
                      -- qualification from import spec we're currently analysing.
                      tell [(ispecQualification, ikModuleName, importedNames')]
                      pure (spec, importedNames')
                  pure $ Just results'

            -- Non-standard code path for breaking import cycles: imported module
            -- is already being loaded. In order to break infinite loop, we must
            -- analyse it here and get all the names we interested in, whithout
            -- resolving the module!
            Just (toLoad, _alreadyLoaded) -> do
              let toResolve = filter ((unresFile /=) . modFile) $ toList toLoad
              logDebug $ "[resolveModule.resolveImports.resolveImport] Module" <+>
                pretty (ikModuleName key) <+>
                "is already being loaded. This happened while loading" <+> pretty
                (mhModName (modHeader mod)) <+> "from" <+> pretty (modFile mod) <> ". Initiating cycle resolution procedures for modules" ##
                pretty (map modFile toResolve)
              Just <$> quasiResolveImportSpecWithLoadsInProgress
                (lift . checkIfModuleIsAlreadyBeingLoaded)
                (lift . readAndLoad)
                (mhModName header)
                (modFile mod)
                key
                toResolve
                importSpecs

    resolveExportedNames
      :: WithCallStack
      => UnresolvedModule -> m SymbolMap
    resolveExportedNames Module{modHeader = ModuleHeader{mhImports, mhModName}, modAllSymbols} = do
      logDebug $ "[resolveModule.resolveExportedNames] Resolving symbols of module" <+> pretty mhModName
      -- logVerboseDebug $ "[resolveModule.resolveExportedNames] Resolved imports" ## ppSubkeyMapWith pretty pretty ppNE resolvedImports
      case mhExports header of
        NoExports
          -> pure modAllSymbols
        EmptyExports
          -> pure modAllSymbols
        SpecificExports ModuleExports{meExportedEntries, meReexports}
          | S.null meReexports
          , let exportedNames :: [EntryWithChildren PosAndType (SymbolName, PosAndType)]
                exportedNames = foldMap toList $ KM.elems meExportedEntries
          , let unqualifiedExportedNames :: [EntryWithChildren PosAndType UnqualifiedSymbolName]
                unqualifiedExportedNames  = mapMaybe (traverse (mkUnqualifiedSymbolName . fst)) exportedNames
                unqualifiedExportedNames' = S.fromList $ map entryName unqualifiedExportedNames
          , S.size unqualifiedExportedNames' == KM.size meExportedEntries
          , SM.isSubsetNames unqualifiedExportedNames' modAllSymbols
          -> do
            TagsServerConf{tsconfNameResolution} <- ask
            let extra            = inferExtraParents header
                modAllSymbols'   = SM.registerChildren extra modAllSymbols
                children :: [Set UnqualifiedSymbolName]
                (errs, children) =
                  partitionEithers $
                  map
                    (childrenNamesFromEntry tsconfNameResolution mhModName modAllSymbols')
                    unqualifiedExportedNames
            traverse_ CME.throwError errs
            pure
              $ SM.restrictKeys modAllSymbols'
              $ unqualifiedExportedNames' <> fold children
          | otherwise
          -> do
            (resolvedImports, namesAndQualifiersFromImports) <- resolveImports mhImports
            let resolveSpecificExports :: NameResolutionStrictness -> Eval (SymbolMap, [ErrorMessage], SymbolMap)
                resolveSpecificExports nameResolution = do
                  (extra :: Map UnqualifiedSymbolName (Set UnqualifiedSymbolName)) <- rpar
                    $ inferExtraParents header
                  (modAllSymbols' :: SymbolMap) <- rpar $ SM.registerChildren extra modAllSymbols
                  (namesInScopeByNamespace :: Map (Maybe ImportQualifier) SymbolMap) <- rpar
                    $ M.fromListWith (<>)
                    $ (Nothing, modAllSymbols')
                    : (Just $ mkImportQualifier mhModName, modAllSymbols')
                    : concatMap expandImportQualification namesAndQualifiersFromImports
                  let lt, gt :: Set ModuleName
                      (lt, reexportsItself, gt) = S.splitMember mhModName meReexports
                  -- Names exported via module reexports.
                  (reexports :: SymbolMap) <- rpar
                    $ resolveReexports resolvedImports
                    $ Pair lt gt
                  -- Names exported from current module, grouped by export qualifier and
                  -- resolving to a location within the export list.
                  (exportedFromExportList' :: [Either ErrorMessage (MonoidalMap (Maybe ImportQualifier) (Map UnqualifiedSymbolName ResolvedSymbol))]) <-
                    parList (evalTraversable rseq) $ flip map (concatMap toList $ KM.elems meExportedEntries) $ \(entry :: EntryWithChildren PosAndType (SymbolName, PosAndType)) ->
                      let (name, PosAndType posFile posLine typ) = entryName entry
                          name' :: UnqualifiedSymbolName
                          (qualifier, name') = splitQualifiedPart name
                      in case M.lookup qualifier namesInScopeByNamespace of
                        Nothing -> throwErrorWithCallStack $ ppDictHeader (PP.hsep
                          [ "Internal error: export qualifier"
                          , PP.dquotes $ pretty qualifier
                          , "has no corresponding qualified imports"
                          ])
                          [ "import entry"                  --> entry
                          , "namesInScopeByNamespace"       :-> ppMap namesInScopeByNamespace
                          , "namesAndQualifiersFromImports" --> namesAndQualifiersFromImports
                          ]
                        Just sm -> do
                          (extraChildrenExports :: Set UnqualifiedSymbolName) <-
                            childrenNamesFromEntry nameResolution mhModName sm $ name' <$ entry
                          let childrenType :: FastTags.Type
                              childrenType = case typ of
                                FastTags.Type   -> FastTags.Constructor
                                FastTags.Family -> FastTags.Type
                                typ'            -> typ'
                              names :: Map UnqualifiedSymbolName ResolvedSymbol
                              names
                                = M.insert name' (mkResolvedSymbolFromParts posFile posLine name' typ Nothing)
                                $ M.fromSet (\childName -> mkResolvedSymbolFromParts posFile posLine childName childrenType (Just name')) extraChildrenExports
                          pure (MM.singleton qualifier names :: MonoidalMap (Maybe ImportQualifier) (Map UnqualifiedSymbolName ResolvedSymbol))
                  let (errors, xs) = partitionEithers exportedFromExportList'
                  (exportedFromExportList :: MonoidalMap (Maybe ImportQualifier) (Map UnqualifiedSymbolName ResolvedSymbol)) <-
                    foldPar xs
                  (presentExports :: SymbolMap) <- rpar
                      $ fold
                      $ M.intersectionWith SM.restrictKeys namesInScopeByNamespace
                      $ fmap M.keysSet
                      $ MM.unMonoidalMap exportedFromExportList
                  -- logVerboseDebug $ "[resolveExportedNames] exportedFromExportList =" ## ppMonoidalMapWith pretty ppMap exportedFromExportList
                  (allSymbols :: SymbolMap) <- rseq $ fold
                    [ if reexportsItself then modAllSymbols' else mempty
                    , reexports
                    , presentExports
                    ]
                  -- logVerboseDebug $ "[resolveModule.resolveExportedNames] inferred extra parents =" ##
                  --   ppMapWith pretty ppSet extra

                  -- Names from default namespace that are exported but not
                  -- imported/defined locally. We default them to tags that
                  -- come from the corresponding export list.
                  let unresolvedExports :: SymbolMap
                      unresolvedExports =
                        SM.withoutKeys
                          (SM.fromList (foldMap M.elems exportedFromExportList))
                          (SM.keysSet allSymbols)
                  allSymbols' <- rseq $ allSymbols <> unresolvedExports
                  pure (allSymbols', errors, unresolvedExports)
            logDebug $ "[resolveModule.resolveExportedNames] analysing export list of module" <+> pretty mhModName
            -- logVerboseDebug $ "[resolveModule.resolveExportedNames] reexports of module" <+> pretty mhModName <> ":" ## ppSet meReexports
            nameResolution <- asks tsconfNameResolution
            let (allSyms, missingImportErrors, unresolvedAndDefaulted) =
                  runEval $ resolveSpecificExports nameResolution
            traverse_ CME.throwError missingImportErrors
            unless (SM.null unresolvedAndDefaulted) $
              logWarning $ "[resolveExportedNames] unresolved exports (will consider them coming from the export list) for" <+> pretty mhModName <> ":" ## pretty unresolvedAndDefaulted
            pure allSyms

expandImportQualification
  :: forall a. (ImportQualification, ModuleName, a)
  -> [(Maybe ImportQualifier, a)]
expandImportQualification = \case
  (Unqualified, name, x)                   ->
    [(Just $ mkImportQualifier name, x), (Nothing, x)]
  (Qualified q, _, x)                      ->
    [(Just q, x)]
  (BothQualifiedAndUnqualified q, name, x) ->
    [(Just $ mkImportQualifier name, x), (Just q, x), (Nothing, x)]

resolveReexports
  :: (Foldable f, Foldable g)
  => SubkeyMap ImportKey (f (ImportSpec, SymbolMap))
  -> g ModuleName
  -> SymbolMap
resolveReexports resolvedImports modNames =
  foldFor modNames $ \modName ->
    foldFor (SubkeyMap.lookupSubkey modName resolvedImports) $ \(_, imports) ->
      foldFor imports $ \(ImportSpec{ispecQualification}, importedNames) ->
        case ispecQualification of
          -- See https://ro-che.info/articles/2012-12-25-haskell-module-system-p1 for details.
          -- Excerpt from Haskell Report:
          -- ‘The form module M names the set of all entities that are in scope
          --  with both an unqualified name e and a qualified name M.e’.
          Qualified _                   -> mempty
          Unqualified                   -> importedNames
          BothQualifiedAndUnqualified _ -> importedNames

-- | Take modules we're currently loading and try to infer names they're exporting
-- without fully resolving them. This is needed to break import cycles in simple
-- cases, that can be resolved. For now, only two simple cases are considered:
-- 1. Module that exports only names defined locally within it.
-- 2. Module may reexport arbitrary names, but we're only importing names
-- defined locally in the module.
quasiResolveImportSpecWithLoadsInProgress
  :: forall m f t. (WithCallStack, Traversable t, Foldable f)
  => (MonadWriter [(ImportQualification, ModuleName, SymbolMap)] m, MonadError ErrorMessage m, MonadReader TagsServerConf m, MonadLog m)
  => (ImportKey -> m (Maybe (NonEmpty UnresolvedModule, [ResolvedModule])))
  -> (ImportKey -> m (Maybe (NonEmpty ResolvedModule)))
  -> ModuleName                -- ^ Module we're currently analysing.
  -> FullPath 'File
  -> ImportKey                 -- ^ Import of the main module that caused the cycle.
  -> f UnresolvedModule        -- ^ Modules in progress that are being loaded and are going to be anayzed here
  -> t ImportSpec              -- ^ Import specs to resolve
  -> m (t (ImportSpec, SymbolMap))
quasiResolveImportSpecWithLoadsInProgress
  checkIfModuleIsAlreadyBeingLoaded
  readAndLoad
  mainModName
  mainModFile
  mainModImport
  modulesAlreadyLoading
  importSpecs =
  for importSpecs $ \spec@ImportSpec{ispecImportKey = ImportKey{ikModuleName = importedModName}, ispecQualification, ispecImportList} -> do
    names <- processImports $ case ispecImportList of
      -- If there's no import list then ensure that either:
      -- 1. There's no export list and therefore all exported names
      -- must be defined locally.
      -- 2. There's an export list but it exports *only* names
      -- defined locally.
      NoImportList              -> Nothing
      AssumedWildcardImportList -> Nothing
      SpecificImports ImportList{ilEntries, ilImportType} ->
        case ilImportType of
          Imported -> Just ilEntries
          Hidden   -> Nothing
    -- Record which names enter current module's scope under certain
    -- qualification from import spec we're currently analysing.
    tell [(ispecQualification, importedModName, names)]
    pure (spec, names)
  where
    processImports :: Maybe (KeyMap Set (EntryWithChildren () UnqualifiedSymbolName)) -> m SymbolMap
    processImports wantedNames =
      foldForA modulesAlreadyLoading $ \Module{modHeader = ModuleHeader{mhExports, mhModName = importedModName, mhImports}, modFile = importedModFile, modAllSymbols} ->
        case mhExports of
          NoExports    -> pure modAllSymbols
          EmptyExports -> pure modAllSymbols
          SpecificExports ModuleExports{meReexports, meExportedEntries}
            | S.null meReexports || allWantedNamesDefinedLocally
            , S.size unqualifiedExports == S.size exportedNames ->
              case wantedSet of
                Nothing -> pure $ modAllSymbols `SM.restrictKeys` unqualifiedExports
                Just wanted
                  | wanted `S.isSubsetOf` unqualifiedExports
                  -> pure $ modAllSymbols `SM.restrictKeys` wanted
                  | otherwise
                  -> pure mempty
                    -- -- TODO: decide whether to actually throw an error or return mempty.
                    -- -- pure mempty -- This module could not have been imported in reality - discard it.
                    -- throwErrorWithCallStack $ ppDictHeader
                    --   "This module could not have been imported in reality"
                    --   [ "importedModName"    --> importedModName
                    --   , "expected"           :-> ppSet wanted
                    --   , "meReexports"        :-> ppSet meReexports
                    --   , "unqualifiedExports" :-> ppSet unqualifiedExports
                    --   , "exportedNames"      :-> ppSet exportedNames
                    --   ]
            -- If all we have are reexports...
            | Just wantedNames' <- wantedNames
            , not (S.null meReexports) && not allWantedNamesDefinedLocally
            , S.null exportedNames -> do
            -- , S.size unqualifiedExports == S.size exportedNames ->
              let reexportedImports :: [ImportSpec]
                  reexportedImports =
                    foldMap (foldMap toList) $ M.restrictKeys (SubkeyMap.toSubmap mhImports) meReexports
                  candidateReexports :: [ImportSpec]
                  candidateReexports =
                    filter (canBringNamesIntoScope wantedNames') reexportedImports

              (canLoad :: [ImportSpec], cannotLoad :: [UnresolvedModule]) <-
                foldForA candidateReexports $ \imp@ImportSpec{ispecImportKey} -> do
                  isBeingLoaded <- checkIfModuleIsAlreadyBeingLoaded ispecImportKey
                  pure $ case isBeingLoaded of
                    -- Not being loaded, may load
                    Nothing ->
                      ([imp], mempty)
                    -- Already being loaded - too bad
                    Just (inProgress, _finishedLoading) ->
                      -- TODO: use '_finishedLoading' as it may help to
                      -- resolve some names.
                      (mempty, toList inProgress)
              (wantedNames'', resolvedNames) <- resolveLoop readAndLoad wantedNames' canLoad

              if not $ S.null wantedNames''
              then do
                logDebug $ ppDictHeader "Import cycle debug info"
                  [ "mainModName"        --> mainModName
                  , "importedModName"    --> importedModName
                  , "canLoad"            --> canLoad
                  , "cannotLoad"         --> (mhModName . modHeader <$> cannotLoad)
                  , "meReexports"        :-> ppSet meReexports
                  , "wantedNames'"       :-> ppKeyMapWith pretty pretty wantedNames'
                  , "reexportedImports"  --> reexportedImports
                  , "candidateReexports" --> candidateReexports
                  ]
                throwErrorWithCallStack $ ppDictHeader
                  (PP.hsep
                    [ "Cannot resolve reexport import cycle: module"
                    , PP.dquotes (pretty mainModName)
                    , "imports names from"
                    , pretty mainModImport
                    , "that reexports names from modules that are already being loaded:"
                    ])
                  [ "Wanted names"   :-> ppKeyMapWith pretty pretty wantedNames'
                  , "Unable to load" --> (mhModName . modHeader <$> cannotLoad)
                  ]
                else pure resolvedNames
            | otherwise -> do
              nameResolution <- asks tsconfNameResolution
              case nameResolution of
                NameResolutionStrict -> throwErrorWithCallStack errMsg
                NameResolutionLax    -> pure mempty
            where
              wantedSet :: Maybe (Set UnqualifiedSymbolName)
              wantedSet = KM.keysSet <$> wantedNames
              allWantedNamesDefinedLocally =
                maybe False (`SM.isSubsetNames` modAllSymbols) wantedSet

              exportedNames :: Set SymbolName
              exportedNames = KM.keysSet meExportedEntries
              unqualifiedExports :: Set UnqualifiedSymbolName
              unqualifiedExports
                = S.mapMonotonic fromJust
                $ S.delete Nothing
                $ S.map mkUnqualifiedSymbolName exportedNames
              errMsg :: Doc Void
              errMsg = ppFoldableHeader
                ("Cannot resolve import cycle: module" <+> PP.dquotes (pretty mainModName) <+> PP.parens (pretty mainModFile) <+>
                "imports names from" <+> pretty (ikModuleName mainModImport) <>
                 ", but a candidate for that import," <+> pretty importedModName <+> PP.parens (pretty importedModFile) <> ", exports names that are not defined locally:")
                (exportedNames `S.difference` S.mapMonotonic getUnqualifiedSymbolName (SM.keysSet modAllSymbols))

resolveLoop
  :: forall m. Monad m
  => (ImportKey -> m (Maybe (NonEmpty ResolvedModule)))
  -> KeyMap Set (EntryWithChildren () UnqualifiedSymbolName)
  -> [ImportSpec]
  -> m (Set UnqualifiedSymbolName, SymbolMap)
resolveLoop readAndLoad wantedNames = go (KM.keysSet wantedNames) mempty
  where
    go
      :: Set UnqualifiedSymbolName
      -> SymbolMap
      -> [ImportSpec]
      -> m (Set UnqualifiedSymbolName, SymbolMap)
    go wanted found = \case
      _ | S.null wanted -> pure (wanted, found)
      []                -> pure (wanted, found)
      ImportSpec{ispecImportKey} : specs -> do
        mod <- readAndLoad ispecImportKey
        case mod of
          Nothing   -> go wanted  found  specs
          Just mods -> go wanted' found' specs
            where
              found' :: SymbolMap
              found' = foldMap ((`SM.restrictKeys` wanted) . modAllExportedNames) mods
              wanted' = wanted S.\\ SM.keysSet found'

canBringNamesIntoScope
  :: KeyMap Set (EntryWithChildren () UnqualifiedSymbolName)
  -> ImportSpec
  -> Bool
canBringNamesIntoScope names ImportSpec{ispecImportList} =
  case ispecImportList of
    NoImportList                                        -> True
    AssumedWildcardImportList                           -> True
    SpecificImports ImportList{ilEntries, ilImportType} ->
      case ilImportType of
        Imported ->
          or $ M.intersectionWith
            canBringNamesViaImport
            (S.map entryChildrenVisibility <$> KM.toMap names)
            (S.map entryChildrenVisibility <$> KM.toMap ilEntries)
        Hidden   ->
          not $ M.null $ M.differenceWith
            hideNames
            (S.map entryChildrenVisibility <$> KM.toMap names)
            (S.map entryChildrenVisibility <$> KM.toMap ilEntries)
      where
        canBringNamesViaImport
          :: Set (Maybe (ChildrenVisibility ()))
          -> Set (Maybe (ChildrenVisibility ()))
          -> Bool
        canBringNamesViaImport wanted present =
          Nothing `S.member` wanted && Nothing `S.member` present ||
          presentWildcards ||
          not (S.null (wantedChildren `S.intersection` presentChildren))
          where
            wantedChildren, presentChildren :: Set UnqualifiedSymbolName
            (Monoid.Any presentWildcards, presentChildren) = foldMap analyse present
            (_,                           wantedChildren)  = foldMap analyse wanted
        hideNames
          :: Set (Maybe (ChildrenVisibility ()))
          -> Set (Maybe (ChildrenVisibility ()))
          -> Maybe (Set (Maybe (ChildrenVisibility ())))
        hideNames wanted present
          | Nothing `S.member` present || presentWildcards
          = Nothing
          | otherwise
          = nothingIfEmpty $ S.map (fmap (hideChildren presentChildren)) $ wanted S.\\ present
          where
            presentChildren :: Set UnqualifiedSymbolName
            (Monoid.Any presentWildcards, presentChildren) = foldMap analyse present
        analyse
          :: Maybe (ChildrenVisibility ())
          -> (Monoid.Any, Set UnqualifiedSymbolName)
        analyse = \case
          Nothing                                 -> (Monoid.Any False, mempty)
          Just VisibleAllChildren                 -> (Monoid.Any True, mempty)
          Just (VisibleSpecificChildren children) -> (mempty, M.keysSet children)
          Just (VisibleAllChildrenPlusSome extra) -> (Monoid.Any True, M.keysSet extra)

hideChildren :: Set UnqualifiedSymbolName -> ChildrenVisibility a -> ChildrenVisibility a
hideChildren toHide = \case
  VisibleAllChildren            -> VisibleAllChildren
  VisibleSpecificChildren xs    -> VisibleSpecificChildren $ M.withoutKeys xs toHide
  VisibleAllChildrenPlusSome xs -> VisibleAllChildrenPlusSome $ M.withoutKeys xs toHide

nothingIfEmpty :: Set a -> Maybe (Set a)
nothingIfEmpty xs
  | S.null xs = Nothing
  | otherwise = Just xs

-- | Some tags should get extra children-parent relationships, that were not
-- evident by looking at tag definitions alone.
-- For instance, in ghc 8.0 exports can be of the form
--
-- > module Mod(FooType(.., Foo', Bar,)) where
--
-- which means that additional pattern synonyms Foo' and Bar' are associated with
-- @FooType@ from now on.
--
-- However, this effect should only be visible in module exports.
-- Within module, there should be no such link between extra children
-- and a parent.
inferExtraParents :: ModuleHeader -> Map UnqualifiedSymbolName (Set UnqualifiedSymbolName)
inferExtraParents ModuleHeader{mhExports} = M.fromListWith (<>) entries
  where
    entries :: [(UnqualifiedSymbolName, Set UnqualifiedSymbolName)]
    entries =
      foldFor mhExports $ \ModuleExports{meExportedEntries} ->
        foldFor meExportedEntries $ \EntryWithChildren{entryName = (name, _), entryChildrenVisibility} ->
          case entryChildrenVisibility of
            Just VisibleAllChildren                         -> mempty
            Just (VisibleSpecificChildren _)                -> mempty
            Just (VisibleAllChildrenPlusSome extraChildren) ->
              [(snd $ splitQualifiedPart name, M.keysSet extraChildren)]
            Nothing                                         -> mempty

-- | Find out which names under qualification come out of an import spec.
visibleNamesFromImportSpec
  :: WithCallStack
  => NameResolutionStrictness
  -> ModuleName   -- ^ Module name where resolution takes place for error reporting.
  -> SymbolMap    -- ^ All names from the set of imports.
  -> ImportSpec   -- ^ Import spec for this particular set of imports.
  -> Either ErrorMessage SymbolMap
visibleNamesFromImportSpec nameResolution moduleName allImportedNames ImportSpec{ispecImportList} =
  case ispecImportList of
    NoImportList                                        ->
      pure allImportedNames
    AssumedWildcardImportList                           ->
      pure allImportedNames
    SpecificImports ImportList{ilImportType, ilEntries} -> do
      let f = case ilImportType of
                Imported -> SM.restrictKeys
                Hidden   -> SM.withoutKeys
      importedNames <- foldMapA (allNamesFromEntry nameResolution moduleName allImportedNames) ilEntries
      pure $ f allImportedNames importedNames

-- | Get names referred to by @EntryWithChildren@ given a @SymbolMap@
-- of names currently in scope.
allNamesFromEntry
  :: WithCallStack
  => NameResolutionStrictness
  -> ModuleName   -- ^ Module name where resolution takes place for error reporting.
  -> SymbolMap
  -> EntryWithChildren ann UnqualifiedSymbolName
  -> Either ErrorMessage (Set UnqualifiedSymbolName)
allNamesFromEntry nameResolution moduleName allImportedNames entry@(EntryWithChildren sym _) =
  S.insert sym <$>
  childrenNamesFromEntry nameResolution moduleName allImportedNames entry

-- | Get all children names referred to by the @EntryWithChildren@ given
-- a @SymbolMap@ of names currently in scope.
childrenNamesFromEntry
  :: WithCallStack
  => NameResolutionStrictness
  -> ModuleName   -- ^ Module name where resolution takes place for error reporting.
  -> SymbolMap
  -> EntryWithChildren ann UnqualifiedSymbolName
  -> Either ErrorMessage (Set UnqualifiedSymbolName)
childrenNamesFromEntry nameResolution moduleName allImportedNames (EntryWithChildren sym childrenVisibility) =
  case childrenVisibility of
    Nothing                                         -> pure mempty
    Just VisibleAllChildren                         -> childrenSymbols
    Just (VisibleSpecificChildren children)         -> pure $ M.keysSet children
    Just (VisibleAllChildrenPlusSome extraChildren) ->
      (M.keysSet extraChildren <>) <$> childrenSymbols
  where
    childrenSymbols
      :: (WithCallStack, MonadError ErrorMessage m)
      => m (Set UnqualifiedSymbolName)
    childrenSymbols =
      case (nameResolution, SM.lookupChildren sym allImportedNames) of
        (NameResolutionStrict, Nothing)       ->
          throwErrorWithCallStack $
            "Imported symbol with children" <+> PP.squotes (pretty sym) <+>
            "is missing from the imports symbol map for the module" <+> PP.squotes (pretty moduleName)
        (NameResolutionLax,    Nothing)       -> pure mempty
        (_,                    Just children) -> pure children
