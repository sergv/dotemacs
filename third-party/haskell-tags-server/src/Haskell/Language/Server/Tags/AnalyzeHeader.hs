----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.AnalyzeHeader
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday, 22 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Haskell.Language.Server.Tags.AnalyzeHeader
  ( analyzeHeader
  ) where

import Control.Arrow (first, second)
import Control.Category ((>>>))
import Control.Monad.Except.Ext
import Control.Monad.Trans.Maybe

import Data.Char
import Data.Foldable.Ext (toList, foldFor)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Strict.Pair as Strict
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Combinators
import Data.Text.Prettyprint.Doc.Ext
import Data.Void (Void)

import Haskell.Language.Lexer.FastTags
  (stripNewlines, tokToName, Pos(..), Line, SrcPos(..), Type, posFile, posLine, unLine, PragmaType(..), ServerToken(..), Type(..))
import qualified Haskell.Language.Lexer.FastTags as FastTags

import Control.Monad.Logging
import Data.ErrorMessage
import Data.KeyMap (KeyMap)
import qualified Data.KeyMap as KM
import Data.Path
import Data.SubkeyMap (SubkeyMap)
import qualified Data.SubkeyMap as SubkeyMap
import Data.Symbols
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

analyzeHeader
  :: (WithCallStack, MonadError ErrorMessage m, MonadLog m)
  => FullPath 'File
  -> [Pos ServerToken]
  -> m (Maybe ModuleHeader, [Pos ServerToken])
analyzeHeader filename ts =
  -- logDebug $ "[analyzeHeader] ts =" <+> ppTokens ts
  case dropWhile ((/= KWModule) . valOf) ts of
    Pos _ KWModule :
      (dropNLs -> Pos _ (T modName) :
        (break ((== KWWhere) . valOf) . dropNLs -> (exportList, Pos _ KWWhere : body))) -> do
      (importSpecs, importQualifiers, body') <- analyzeImports filename mempty mempty body
      exports                                <- analyzeExports filename importQualifiers exportList
      let header = ModuleHeader
            { mhModName          = mkModuleName modName
            , mhImports          = importSpecs
            , mhImportQualifiers = importQualifiers
            , mhExports          = exports
            }
      pure (Just header, body')
      -- No header present.
    _ -> pure (Nothing, ts)

pattern PAs           :: Pos ServerToken
pattern PAs           <- Pos _ (T "as")
pattern PComma        :: Pos ServerToken
pattern PComma        <- Pos _ Comma
pattern PHiding       :: Pos ServerToken
pattern PHiding       <- Pos _ (T "hiding")
pattern PImport       :: Pos ServerToken
pattern PImport       <- Pos _ KWImport
pattern PForeign      :: Pos ServerToken
pattern PForeign      <- Pos _ KWForeign
pattern PLParen       :: Pos ServerToken
pattern PLParen       <- Pos _ LParen
pattern PModule       :: Pos ServerToken
pattern PModule       <- Pos _ KWModule
pattern PName         :: Text -> Pos ServerToken
pattern PName name    <- Pos _ (T name)
pattern PNewline      :: Int -> Pos ServerToken
pattern PNewline x    <- Pos _ (Newline x)
pattern PPattern      :: Pos ServerToken
pattern PPattern      <- Pos _ (T "pattern")
pattern PQualified    :: Pos ServerToken
pattern PQualified    <- Pos _ (T "qualified")
pattern PRParen       :: Pos ServerToken
pattern PRParen       <- Pos _ RParen
pattern PSourcePragma :: Pos ServerToken
pattern PSourcePragma <- Pos _ (Pragma SourcePragma)
pattern PString       :: Pos ServerToken
pattern PString       <- Pos _ String
pattern PType         :: Pos ServerToken
pattern PType         <- Pos _ KWType

pattern PAnyName      :: Text -> Pos ServerToken
pattern PAnyName name <- Pos _ (tokToName -> Just name)

pattern PName'             :: SrcPos -> Text -> Pos ServerToken
pattern PName' pos name    <- Pos pos (T name)
pattern PAnyName'          :: SrcPos -> Text -> Pos ServerToken
pattern PAnyName' pos name <- Pos pos (tokToName -> Just name)

analyzeImports
  :: forall m. (WithCallStack, MonadError ErrorMessage m, MonadLog m)
  => FullPath 'File
  -> SubkeyMap ImportKey (NonEmpty ImportSpec)
  -> Map ImportQualifier (NonEmpty ModuleName)
  -> [Pos ServerToken]
  -> m ( SubkeyMap ImportKey (NonEmpty ImportSpec)
       , Map ImportQualifier (NonEmpty ModuleName)
       , [Pos ServerToken]
       )
analyzeImports filename imports qualifiers ts = do
  -- logDebug $ "[analyzeImports] ts =" <+> ppTokens ts
  let d = dropNLs
  res <- runMaybeT $ do
    -- Drop initial "import" keyword and {-# SOURCE #-} pragma, if any
    (ts2, importTarget) <- case dropWhile (\case { PImport -> False; PForeign -> False; _ -> True }) $ d ts of
      PImport  : (d -> PSourcePragma : rest) -> pure (rest, HsBootModule)
      PImport  :                       rest  -> pure (rest, VanillaModule)
      _                                      -> mzero
    let dropSafeImport = \case
          PName "safe" : rest -> rest
          rest                -> rest
        dropPackageImport = \case
          PString : rest      -> rest
          rest                -> rest
        extractQualified = \case
          PQualified : rest -> (rest, True)
          rest              -> (rest, False)

        (ts3, isQual)
          = first (d >>> dropPackageImport)
          . extractQualified
          . d
          . dropSafeImport
          . d
          $ ts2
    -- Extract import name and renaming alias, if any
    (ts4, name, qualName) <- case d ts3 of
      PName name : (d -> PAs : (d -> PName qualName : rest)) -> pure (rest, name, Just qualName)
      PName name :                                    rest   -> pure (rest, name, Nothing)
      _                                                      -> mzero
    -- Make sence of the data collected before
    let qualType = case (isQual, qualName) of
          (True,  Nothing)        -> Qualified $ mkQual name
          (True,  Just qualName') -> Qualified $ mkQual qualName'
          (False, Nothing)        -> Unqualified
          (False, Just qualName') -> BothQualifiedAndUnqualified $ mkQual qualName'
    pure (name, qualType, importTarget, ts4)
  case res of
    Nothing                                  -> pure (imports, qualifiers, ts)
    Just (name, qualType, importTarget, ts5) -> add name qualType importTarget ts5
  where
    mkQual = mkImportQualifier . mkModuleName
    add
      :: Text
      -> ImportQualification
      -> ImportTarget
      -> [Pos ServerToken]
      -> m ( SubkeyMap ImportKey (NonEmpty ImportSpec)
           , Map ImportQualifier (NonEmpty ModuleName)
           , [Pos ServerToken]
           )
    add name qual importTarget toks = do
      (importList, toks') <- analyzeImportList toks
      let spec     = mkNewSpec importList
          imports' = SubkeyMap.insertWith
                       (<>)
                       (ImportKey importTarget modName)
                       (spec :| [])
                       imports
      analyzeImports filename imports' qualifiers' toks'
      where
        modName :: ModuleName
        modName = mkModuleName name
        qualifiers' :: Map ImportQualifier (NonEmpty ModuleName)
        qualifiers' = case getQualifier qual of
                        Just q  -> M.insertWith (<>) q (modName :| []) qualifiers
                        Nothing -> qualifiers
        mkNewSpec :: ImportListSpec ImportList -> ImportSpec
        mkNewSpec importList = ImportSpec
          { ispecImportKey     = ImportKey
              { ikModuleName   = modName
              , ikImportTarget = importTarget
              }
          , ispecQualification = qual
          , ispecImportList    = importList
          }
    -- Analyze comma-separated list of entries, starting at _|_:
    -- - Foo_|_
    -- - Foo_|_(Bar, Baz)
    -- - Foo_|_ hiding (Bar, Baz)
    -- - Quux_|_(..)
    analyzeImportList :: [Pos ServerToken] -> m (ImportListSpec ImportList, [Pos ServerToken])
    analyzeImportList toks = do
      -- logDebug $ "[analyzeImpotrList] toks =" <+> ppTokens toks
      case lastNL toks of
        PNewline 0 : _ -> pure (NoImportList, toks)
        toks'          ->  case dropNLs toks' of
          []                                    -> pure (NoImportList, toks)
          PHiding : (dropNLs -> PLParen : rest) -> findImportListEntries Hidden mempty (dropNLs rest)
          PLParen : rest                        -> findImportListEntries Imported mempty (dropNLs rest)
          _                                     -> pure (NoImportList, toks)
    findImportListEntries
      :: ImportType
      -> KeyMap Set (EntryWithChildren () UnqualifiedSymbolName)
      -> [Pos ServerToken]
      -> m (ImportListSpec ImportList, [Pos ServerToken])
    findImportListEntries importType acc toks' = do
      -- logDebug $ "[findImportListEntries] toks =" <+> ppTokens toks
      case dropNLs toks' of
        []                                                                ->
          pure (SpecificImports importList, [])
        -- Reaching here means tricks with preprocessor which we cannot
        -- reasonably handle. E.g.
        --
        -- > import Foo
        -- > #ifdef FOO
        -- >   ( foo
        -- >   , bar
        -- > #else
        -- >   ( baz
        -- >   , quux
        -- > #endif
        -- >   , fizz
        -- >   )
        rest@(PImport : _)                                                ->
          pure (SpecificImports importList, rest)
        PRParen : rest                                                    ->
          pure (SpecificImports importList, rest)
        -- Type import
        PType : PName name : rest                                         ->
          entryWithoutChildren name rest
        PType : PLParen : PAnyName name : PRParen : rest                  ->
          entryWithoutChildren name rest
        -- Pattern import
        PPattern : restWithName@(PName name : rest)
          | isVanillaTypeName name
          , not $ isChildrenList filename rest ->
            entryWithoutChildren name rest
          | otherwise                          ->
            entryWithoutChildren "pattern" restWithName
        PPattern : restWithName@(PLParen : PAnyName name : PRParen : rest)
          | isOpTypeName name
          , not $ isChildrenList filename rest ->
            entryWithoutChildren name rest
          | otherwise                          ->
            entryWithoutChildren "pattern" restWithName
        -- Vanilla function/operator/consturtor/type import
        PLParen : PAnyName name : PRParen : rest                          ->
          entryWithChildren "operator in import list" name rest
        PLParen : PName name : PRParen : rest                             ->
          entryWithChildren "operator in import list" name rest
        PName name : rest                                                 ->
          entryWithChildren "name in import list" name rest
        PLParen : rest                                                    ->
          findImportListEntries importType acc rest
        Pos _ HSC2HS : rest                                               -> do
          -- We cannot run hsc2hs here so we'll conservatively
          -- assume that everything is imported from a module.
          (_, remaining) <- findImportListEntries importType mempty $ dropCommas rest
          pure (AssumedWildcardImportList, remaining)
        rest                                                              ->
          throwErrorWithCallStack $ "Unrecognised shape of import list:" ## ppTokens rest
      where
        importList :: ImportList
        importList = ImportList
          { ilEntries    = acc
          , ilImportType = importType
          }
        entryWithChildren
          :: Doc Void
          -> Text
          -> [Pos ServerToken]
          -> m (ImportListSpec ImportList, [Pos ServerToken])
        entryWithChildren descr name rest = do
          (children, rest') <- snd $ analyzeChildren descr filename $ dropNLs rest
          name'             <- mkUnqualName name
          let newEntry = EntryWithChildren name' $ (() <$) <$> children
          findImportListEntries importType (KM.insert newEntry acc) $ dropCommas rest'
        entryWithoutChildren :: Text -> [Pos ServerToken] -> m (ImportListSpec ImportList, [Pos ServerToken])
        entryWithoutChildren name rest = do
          name' <- mkUnqualName name
          let newEntry = mkEntryWithoutChildren name'
          findImportListEntries importType (KM.insert newEntry acc) $ dropCommas rest
        mkUnqualName :: Text -> m UnqualifiedSymbolName
        mkUnqualName name =
          case mkUnqualifiedSymbolName (mkSymbolName name) of
            Nothing    ->
              throwErrorWithCallStack $ "Invalid qualified entry on import list:" <+> docFromText name
            Just name' -> pure name'

analyzeExports
  :: forall m. (WithCallStack, MonadError ErrorMessage m, MonadLog m)
  => FullPath 'File
  -> Map ImportQualifier (NonEmpty ModuleName)
  -> [Pos ServerToken]
  -> m (ModuleExportSpec ModuleExports)
analyzeExports filename importQualifiers ts = do
  -- logDebug $ "[analyzeExports] ts =" <+> ppTokens ts
  case stripNewlines ts of
    []                        -> pure NoExports
    -- Drop any other module declarations that could have arisen thanks to e.g. CPP.
    PModule : PName _ : rest -> analyzeExports filename importQualifiers rest
    PLParen : PRParen : _    -> pure EmptyExports
    PLParen : rest           -> SpecificExports <$> go mempty mempty rest
    toks                     ->
      throwErrorWithCallStack $ "Unrecognised shape of export list:" ## ppTokens toks
  where
    -- Analyze comma-separated list of entries like
    -- - Foo
    -- - Foo(Bar, Baz)
    -- - Quux(..)
    -- - pattern PFoo
    -- - module Data.Foo.Bar
    go :: KeyMap NonEmpty (EntryWithChildren PosAndType (SymbolName, PosAndType))
       -> Set ModuleName
       -> [Pos ServerToken]
       -> m ModuleExports
    go entries reexports toks = do
      -- logDebug $ "[analyzeExports.go] toks =" <+> ppTokens toks
      case toks of
        [] ->
          pure exports
        PRParen : _ ->
          pure exports
        -- Pattern export
        PPattern : restWithName@(PName' SrcPos{posLine} name : rest)
          | isVanillaTypeName name
          , not $ isChildrenList filename rest ->
            entryWithoutChildren name posLine FastTags.Pattern rest
          | otherwise                 ->
            entryWithoutChildren "pattern" posLine FastTags.Function restWithName
        PPattern : restWithName@(PLParen : PAnyName' SrcPos{posLine} name : PRParen : rest)
          | isOpTypeName name
          , not $ isChildrenList filename rest ->
            entryWithoutChildren name posLine FastTags.Pattern rest
          | otherwise                 ->
            entryWithoutChildren "pattern" posLine FastTags.Function restWithName
        -- Type export
        PType : PName' SrcPos{posLine} name : rest ->
          entryWithoutChildren name posLine FastTags.Family rest
        PType : PLParen : PAnyName' SrcPos{posLine} name : PRParen : rest ->
          entryWithoutChildren name posLine FastTags.Family rest
        -- Module reexport
        PModule : PName name : rest ->
          consumeComma entries (newReexports <> reexports) rest
          where
            modName = mkModuleName name
            newReexports :: Set ModuleName
            newReexports
              = S.fromList
              $ toList
              $ M.findWithDefault (modName :| []) (mkImportQualifier modName) importQualifiers
        -- Vanilla function/operator/consturtor/type export
        PLParen : PName' SrcPos{posLine} name : PRParen : rest ->
          entryWithChildren "operator in export list" name posLine (typeForName FastTags.Type name) rest
        PLParen : Pos SrcPos{posLine} (tokToName -> Just name) : PRParen : rest ->
          entryWithChildren "operator in export list" name posLine (typeForName FastTags.Type name) rest
        PName' SrcPos{posLine} name : rest ->

          entryWithChildren "name in export list" name posLine (typeForName FastTags.Type name) rest
        PLParen : rest ->
          go entries reexports rest
        toks' ->
          throwErrorWithCallStack $ "Unrecognised export list structure:" <+> ppTokens toks'
      where
        exports :: ModuleExports
        exports = ModuleExports
          { meExportedEntries    = entries
          , meReexports          = reexports
          , meHasWildcardExports = getAny $ foldMap exportsAllChildren entries
          }
        entryWithChildren
          :: Doc Void
          -> Text
          -> Line
          -> Type
          -> [Pos ServerToken]
          -> m ModuleExports
        entryWithChildren listType name !line typIfNoChildren rest = do
          -- logDebug $ "[analyzeExports.entryWithChildren] rest =" <+> ppTokens rest
          let (presence, getChildren) = analyzeChildren listType filename rest
          (children, rest') <- getChildren
          entryType <-
            case ( presence
                 , foldFor children $ foldMap $ \PosAndType{patType} ->
                     Strict.Pair (Any (patType == Constructor)) (Any (patType == Type))) of
              (ChildrenAbsent,  _)                      -> pure typIfNoChildren
              (ChildrenPresent, Strict.Pair (Any False) (Any False)) -> pure Type
              (ChildrenPresent, Strict.Pair (Any True)  (Any False)) -> pure Type
              (ChildrenPresent, Strict.Pair (Any False) (Any True))  -> pure Family
              (ChildrenPresent, Strict.Pair (Any True)  (Any True))  -> throwErrorWithCallStack $
                "Unexpected children specification for exported name" <+> PP.squotes (pretty name) <> "." <+>
                "It specifies both constructor names and type names among children:" ##
                  ppTokens rest
          let newEntry = EntryWithChildren (mkSymbolName name, PosAndType filename line entryType) children
          consumeComma (KM.insert newEntry entries) reexports rest'
        entryWithoutChildren
          :: Text
          -> Line
          -> Type
          -> [Pos ServerToken]
          -> m ModuleExports
        entryWithoutChildren name !line typ rest = do
          -- logDebug $ "[analyzeExports.entryWithoutChildren] rest =" <+> ppTokens rest
          let newEntry = mkEntryWithoutChildren (mkSymbolName name, PosAndType filename line typ)
          consumeComma (KM.insert newEntry entries) reexports rest
        exportsAllChildren :: EntryWithChildren PosAndType a -> Any
        exportsAllChildren (EntryWithChildren _ visibility) =
          maybe mempty isExportAllChildren visibility
          where
            isExportAllChildren VisibleAllChildren             = Any True
            isExportAllChildren (VisibleSpecificChildren _)    = mempty
            isExportAllChildren (VisibleAllChildrenPlusSome _) = Any True

    -- Continue parsing by consuming comma delimiter.
    consumeComma
      :: KeyMap NonEmpty (EntryWithChildren PosAndType (SymbolName, PosAndType))
      -> Set ModuleName
      -> [Pos ServerToken]
      -> m ModuleExports
    consumeComma entries reexports = go entries reexports . dropCommas

typeForName :: FastTags.Type -> Text -> FastTags.Type
typeForName constructorLikeTag name =
  case T.uncons $ unqualSymNameText $ stripQualifiedPart name of
    Just (':', _) -> constructorLikeTag
    Just (c, _)
      | isAlpha c -> if isUpper c then constructorLikeTag else FastTags.Function
      | otherwise -> FastTags.Operator
    Nothing -> FastTags.Function

isChildrenList :: FullPath 'File -> [Pos ServerToken] -> Bool
isChildrenList filename toks =
  case fst (analyzeChildren mempty filename toks :: (ChildrenPresence, Either ErrorMessage (Maybe (ChildrenVisibility PosAndType), [Pos ServerToken]))) of
    ChildrenPresent -> True
    ChildrenAbsent  -> False

data ChildrenPresence = ChildrenPresent | ChildrenAbsent

data WildcardPresence = WildcardPresent | WildcardAbsent

instance Semigroup WildcardPresence where
  (<>) WildcardAbsent y              = y
  (<>) x              WildcardAbsent = x
  (<>) _              _              = WildcardPresent

instance Monoid WildcardPresence where
  mempty = WildcardAbsent
  mappend = (<>)

analyzeChildren
  :: forall m. (WithCallStack, MonadError ErrorMessage m)
  => Doc Void
  -> FullPath 'File
  -> [Pos ServerToken]
  -> (ChildrenPresence, m (Maybe (ChildrenVisibility PosAndType), [Pos ServerToken]))
analyzeChildren listType filename toks =
  case dropNLs toks of
    []                                               -> (ChildrenAbsent, pure (Nothing, []))
    toks'@(PComma : _)                               -> (ChildrenAbsent, pure (Nothing, toks'))
    toks'@(PRParen : _)                              -> (ChildrenAbsent, pure (Nothing, toks'))
    toks'@(PName _ : _)                              -> (ChildrenAbsent, pure (Nothing, toks'))
    toks'@(PModule : _)                              -> (ChildrenAbsent, pure (Nothing, toks'))
    toks'@(PPattern : _)                             -> (ChildrenAbsent, pure (Nothing, toks'))
    toks'@(PType : _)                                -> (ChildrenAbsent, pure (Nothing, toks'))
    -- PLParen : PName ".." : PRParen : rest            -> (ChildrenPresent, pure (Just VisibleAllChildren, rest))
    PLParen : PRParen : rest                         -> (ChildrenAbsent, pure (Nothing, rest))
    PLParen : rest@(PAnyName name : _)
      | isNonOperatorName name       -> analyzeList rest
      | otherwise                    -> (ChildrenAbsent, pure (Nothing, toks))
    PLParen : rest@(PType : PAnyName name : _)
      | isNonOperatorName name       -> analyzeList rest
      | otherwise                    -> (ChildrenAbsent, pure (Nothing, toks))
    PLParen : rest@(PLParen : PAnyName name : PRParen : _)
      | not $ isNonOperatorName name -> analyzeList rest
      | otherwise                    -> (ChildrenAbsent, pure (Nothing, toks))
    PLParen : rest@(PType : PLParen : PAnyName name : PRParen : _)
      | not $ isNonOperatorName name -> analyzeList rest
      | otherwise                    -> (ChildrenAbsent, pure (Nothing, toks))
    toks'                                            ->
      (ChildrenAbsent, throwErrorWithCallStack $ "Cannot handle children of" <+> listType <> ":" ## ppTokens toks')
  where
    analyzeList
      :: WithCallStack
      => [Pos ServerToken]
      -> (ChildrenPresence, m (Maybe (ChildrenVisibility PosAndType), [Pos ServerToken]))
    analyzeList = second (fmap mkVisibility) . extractChildren mempty mempty . dropNLs
      where
        mkVisibility
          :: (Map UnqualifiedSymbolName PosAndType, WildcardPresence, t)
          -> (Maybe (ChildrenVisibility PosAndType), t)
        mkVisibility (children, wildcard, toks') = (visibility, toks')
          where
            visibility
              | M.null children =
                case wildcard of
                  WildcardPresent -> Just VisibleAllChildren
                  WildcardAbsent  -> Just $ VisibleSpecificChildren children
              | otherwise       =
                case wildcard of
                  WildcardPresent -> Just $ VisibleAllChildrenPlusSome children
                  WildcardAbsent  -> Just $ VisibleSpecificChildren children

    extractChildren
      :: WithCallStack
      => WildcardPresence
      -> Map UnqualifiedSymbolName PosAndType
      -> [Pos ServerToken]
      -> (ChildrenPresence, m (Map UnqualifiedSymbolName PosAndType, WildcardPresence, [Pos ServerToken]))
    extractChildren wildcardPresence !names = \case
      []                                                                ->
        (childrenPresence, pure (names, wildcardPresence, []))
      PRParen : rest                                                    ->
        (childrenPresence, pure (names, wildcardPresence, rest))
      PType : PName' SrcPos{posLine} name : rest                        ->
        extractChildren wildcardPresence (M.insert (stripQualifiedPart name) (PosAndType filename posLine Type) names) $ dropCommas rest
      PType : PLParen : PAnyName' SrcPos{posLine} name : PRParen : rest ->
        extractChildren wildcardPresence (M.insert (stripQualifiedPart name) (PosAndType filename posLine Type) names) $ dropCommas rest
      PName ".." : rest                                                 ->
        extractChildren (wildcardPresence <> WildcardPresent) names $ dropCommas rest
      PAnyName' SrcPos{posLine} name : rest                             ->
        extractChildren wildcardPresence (M.insert (stripQualifiedPart name) (PosAndType filename posLine (typeForName Constructor name)) names) $ dropCommas rest
      PLParen : PAnyName' SrcPos{posLine} name : PRParen : rest         ->
        extractChildren wildcardPresence (M.insert (stripQualifiedPart name) (PosAndType filename posLine (typeForName Constructor name)) names) $ dropCommas rest
      PLParen : rest                                                    ->
        extractChildren wildcardPresence names $ dropNLs rest
      toks'                                                             ->
        (ChildrenAbsent, throwErrorWithCallStack $ "Unrecognised children list structure:" ## ppTokens toks')
      where
        childrenPresence
          | M.null names =
            case wildcardPresence of
              WildcardPresent -> ChildrenPresent
              WildcardAbsent  -> ChildrenAbsent
          | otherwise    = ChildrenPresent

stripQualifiedPart :: Text -> UnqualifiedSymbolName
stripQualifiedPart = snd . splitQualifiedPart . mkSymbolName

isNonOperatorName :: Text -> Bool
isNonOperatorName =
  T.all check . unqualSymNameText . snd . splitQualifiedPart . mkSymbolName
  where
    check :: Char -> Bool
    check '\'' = True
    check '_'  = True
    check '#'  = True
    check '.'  = True
    check c    = isAlphaNum c

newtype Tokens = Tokens [Pos ServerToken]

instance Pretty Tokens where
  pretty (Tokens [])       = "[]"
  pretty (Tokens ts@(t : _)) =
    ppDictHeader "Tokens"
      [ "file"   :-> pretty $ posFile $ posOf t
      , "tokens" :-> ppListWith ppTokenVal ts
      ]
    where
      ppTokenVal :: Pos ServerToken -> Doc ann
      ppTokenVal (Pos SrcPos{posLine} tok) =
        pretty (unLine posLine) <> PP.colon <> pretty tok

ppTokens :: [Pos ServerToken] -> Doc ann
ppTokens = pretty . Tokens . take 16

-- | Drop prefix of newlines except the last one.
lastNL :: [Pos ServerToken] -> [Pos ServerToken]
lastNL (PNewline _ : ts@(PNewline _ : _)) = lastNL ts
lastNL ts                                 = ts

-- | Drop prefix of newlines.
dropNLs :: [Pos ServerToken] -> [Pos ServerToken]
dropNLs (PNewline _ : ts) = dropNLs ts
dropNLs ts                = ts

dropCommas :: [Pos ServerToken] -> [Pos ServerToken]
dropCommas = go . dropNLs
  where
    go (Pos _ Comma : ts) = dropCommas ts
    go ts                 = ts

isVanillaTypeName  :: Text -> Bool
isVanillaTypeName = maybe False (isUpper . fst) . T.uncons

isOpTypeName :: Text -> Bool
isOpTypeName = maybe False ((== ':') . fst) . T.uncons

