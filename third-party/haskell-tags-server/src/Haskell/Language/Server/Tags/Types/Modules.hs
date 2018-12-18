----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Server.Tags.Types.Modules
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Haskell.Language.Server.Tags.Types.Modules
  (
    -- * Types for representing Haskell modules
    Module(..)
  , UnresolvedModule
  , ResolvedModule
  , moduleNeedsReloading
  , ModuleHeader(..)
  , resolveQualifier
  , ModuleExportSpec(..)
  , ModuleExports(..)
  , PosAndType(..)
  ) where

import Prelude hiding (mod)

import Control.DeepSeq
import Control.Monad.Except.Ext
import Control.Parallel.Strategies.Ext

import Data.Hashable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Data.Store (Store)
import Data.Text.Prettyprint.Doc.Ext
import Data.Time.Clock (UTCTime(..))
import Data.Traversable (for)
import GHC.Generics (Generic)

import Control.Monad.Filesystem (MonadFS)
import qualified Control.Monad.Filesystem as MonadFS
import Data.ErrorMessage
import Data.KeyMap (KeyMap, HasKey(..))
import Data.Path (FullPath, FileType(..))
import Data.SubkeyMap (SubkeyMap)
import qualified Data.SubkeyMap as SubkeyMap
import Data.SymbolMap (SymbolMap)
import Data.Symbols
import Haskell.Language.Lexer.FastTags (Type, Line)
import Haskell.Language.Server.Tags.Types.Imports

data Module a = Module
  { modHeader           :: !ModuleHeader
    -- | All symbols defined in this module. SymbolMap tracks all children-parent
    -- relationships.
  , modAllSymbols       :: !SymbolMap
    -- | File the module was loaded from.
  , modFile             :: !(FullPath 'File)
    -- | Time as reported by getModificationTime.
  , modLastModified     :: !UTCTime
    -- | All names that this module brings into scope
  , modAllExportedNames :: !a
    -- | Whether some imports of this module were updated and thus revolved
    -- module exports are no longer valid.
  , modIsDirty          :: !Bool
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (Module a)
instance Store  a => Store  (Module a)

type UnresolvedModule = Module ()
type ResolvedModule   = Module SymbolMap

moduleNeedsReloading :: MonadFS m => Module a -> m (Bool, UTCTime)
moduleNeedsReloading Module{modFile, modIsDirty, modLastModified} = do
  modifTime <- MonadFS.getModificationTime modFile
  pure (modIsDirty || modLastModified /= modifTime, modifTime)

instance Pretty a => Pretty (Module a) where
  pretty mod =
    ppDictHeader "Module"
      [ "Name"          --> mhModName $ modHeader mod
      , "File"          --> modFile mod
      , "Last modified" :-> docFromString $ show $ modLastModified mod
      , "Header"        --> modHeader mod
      , "AllSymbols"    --> modAllSymbols mod
      ]

-- | Result of analysing module's export list and any subsequent imports. E.g.
--
-- > module Foo (...) where
-- > import Bar
-- > import qualified Baz hiding (frob)
data ModuleHeader = ModuleHeader
  { mhModName          :: !ModuleName
    -- | Exports of a module. Nothing - everything is exported
  , mhExports          :: !(ModuleExportSpec ModuleExports)
    -- | Mapping from qualifiers to original module names. Single qualifier
    -- may be used for several modules.
  , mhImportQualifiers :: !(Map ImportQualifier (NonEmpty ModuleName))
    -- | All imports of a given module, including qualified ones.
    -- NB same module name may be present several times with different qualifications
    -- because it may be imported several times.
  , mhImports          :: !(SubkeyMap ImportKey (NonEmpty ImportSpec))
  } deriving (Eq, Ord, Show, Generic)

instance NFData ModuleHeader
instance Store  ModuleHeader

instance Pretty ModuleHeader where
  pretty ModuleHeader{mhModName, mhExports, mhImportQualifiers, mhImports} =
    ppDictHeader "Module" $
      [ "Name"             :-> pretty mhModName
      ] ++
      [ "Exports"          :-> pretty exports
      | SpecificExports exports <- [mhExports]
      ] ++
      [ "ImportQualifiers" :-> ppMapWith pretty ppNE $ mhImportQualifiers
      | not $ M.null mhImportQualifiers
      ] ++
      [ "Imports"          :-> ppSubkeyMapWith pretty pretty ppNE $ mhImports
      | not $ SubkeyMap.null mhImports
      ]

-- | Find out which modules a given @ImportQualifier@ refers to.
resolveQualifier
  :: (WithCallStack, MonadError ErrorMessage m)
  => ImportQualifier
  -> ModuleHeader
  -> m (Maybe (NonEmpty (ImportKey, NonEmpty ImportSpec)))
resolveQualifier qual ModuleHeader{mhImports, mhImportQualifiers} =
  case SubkeyMap.lookupSubkey qualifiedModName mhImports of
    []     ->
      case M.lookup qual mhImportQualifiers of
        Nothing    -> pure Nothing
        Just names ->
          fmap (Just . runEval . foldParNE) $ for names $ \modName ->
            case SubkeyMap.lookupSubkey modName mhImports of
              []    ->
                -- If module's not found then this is a violation of internal
                -- invariant of ModuleHeader module header datastructure.
                throwErrorWithCallStack $
                  "Internal error: module" <+> pretty modName <+>
                  "for qualifier" <+> pretty qual <+> "not found in the imports map"
              s:ss -> pure $ s :| ss
    s : ss -> pure $ Just $ s :| ss
  where
    qualifiedModName = getImportQualifier qual

data ModuleExportSpec a =
    -- | Export list completely absent.
    NoExports
  | -- | Export list specifies no entries.
    EmptyExports
  | -- | Exprort list specifies entries.
    SpecificExports !a
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance NFData a => NFData (ModuleExportSpec a)
instance Store  a => Store  (ModuleExportSpec a)

instance Pretty a => Pretty (ModuleExportSpec a) where
  pretty = ppGeneric

data ModuleExports = ModuleExports
  { -- | Toplevel names exported from this particular module as specified in
    -- the header.
    meExportedEntries    :: !(KeyMap NonEmpty (EntryWithChildren PosAndType (SymbolName, PosAndType)))
    -- | Module name here refer to real modules only. I.e. reexports via
    -- handles, like
    --
    -- > module Foo
    -- > ( module Bar
    -- > , module Data.List
    -- > ) where
    -- >
    -- > import Data.Array as Bar
    -- > import Data.List
    --
    -- were already resolved into @Data.Set.fromList ["Data.Array", "Data.List"]@.
  , meReexports          :: !(Set ModuleName)
    -- | Whether this module exports any entities that export all children.
  , meHasWildcardExports :: !Bool
  } deriving (Eq, Ord, Show, Generic)

instance NFData ModuleExports
instance Store  ModuleExports

instance Pretty ModuleExports where
  pretty = ppGeneric

instance Semigroup ModuleExports where
  {-# INLINE (<>) #-}
  (<>) (ModuleExports x y z) (ModuleExports x' y' z') =
    ModuleExports (x <> x') (y <> y') (z || z')

instance Monoid ModuleExports where
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}
  mempty = ModuleExports mempty mempty False
  mappend = (<>)

instance HasKey (EntryWithChildren ann (SymbolName, PosAndType)) where
  type Key (EntryWithChildren ann (SymbolName, PosAndType)) = SymbolName
  {-# INLINE getKey #-}
  getKey = fst . entryName

-- | Tag position and type stored for later analysis
data PosAndType = PosAndType
  { patPosFile  :: !(FullPath 'File)
  , patPosLine  :: {-# UNPACK #-} !Line
  , patType     :: !Type
  } deriving (Eq, Ord, Show, Generic)

instance Hashable PosAndType
instance NFData   PosAndType
instance Store    PosAndType

instance Pretty PosAndType where
  pretty = ppGeneric

