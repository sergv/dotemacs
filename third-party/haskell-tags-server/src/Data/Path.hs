----------------------------------------------------------------------------
-- |
-- Module      :  Data.Path
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday, 10 November 2016
----------------------------------------------------------------------------

{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -Wredundant-constraints          #-}
{-# OPTIONS_GHC -Wsimplifiable-class-constraints #-}

#ifdef mingw32_HOST_OS
#define WINDOWS 1
#endif

module Data.Path
  ( FileType(..)
  , FullPath
  , unFullPath
  , fullPathAsUtf8
  , toFilePath
  , MkFullPath(..)
  , MkSomeFullPath(..)
  , doesFileExist
  , doesDirectoryExist
  , listDirectory
  , getModificationTime
  , splitDirectories
  , PathFragment
  , mkPathFragment
  , mkSinglePathFragment
  , unPathFragment
  , Extension
  , mkExtension
  , unExtension
  -- * Overloaded operations
  , JoinPaths(..)
  , Contains(..)
  , AddExtension(..)
  , DropExtensions(..)
  , TakeFileName(..)
  , TakeDirectory(..)
  , TakeExtension(..)
  , MakeRelative(..)
  -- * Base path
  , BaseName
  , unBaseName
  ) where

import Control.Monad.Base
import Control.Monad.Except.Ext
import Control.Monad.Ext

import qualified Data.ByteString as BS
import Data.Coerce
import Data.ErrorMessage
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup as Semigroup
import Data.Semigroup.Foldable.Class (foldMap1)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Prettyprint.Doc.Ext
import Data.Time.Clock (UTCTime)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

import Data.Path.Internal

#ifdef WINDOWS
#else
import Control.Exception
import qualified Data.Text.Prettyprint.Doc as PP
import System.Posix.Files as Posix
#endif

class MkSomeFullPath a m where
  mkSomeFullPath :: a -> m (Either (FullPath 'File) (FullPath 'Dir))

instance (MonadBase IO m, MonadError ErrorMessage m) => MkSomeFullPath FilePath.FilePath m where
  {-# INLINE mkSomeFullPath #-}
#ifdef WINDOWS
  mkSomeFullPath
    :: FilePath.FilePath
    -> m (Either (FullPath 'File) (FullPath 'Dir))
  mkSomeFullPath path = do
    path'  <- liftBase $ do
      isSymlink <- Directory.pathIsSymbolicLink path
      if isSymlink
      then Directory.makeAbsolute =<< Directory.getSymbolicLinkTarget path
      else Directory.makeAbsolute path
    isFile <- liftBase $ Directory.doesFileExist path'
    if isFile
    then pure $! Left $! FullPath $ T.pack path'
    else do
      isDir <- liftBase $ Directory.doesDirectoryExist path'
      if isDir
      then pure $! Right $! FullPath $ T.pack path'
      else throwErrorWithCallStack $
        "Path does not refer to either a file or a directory:" <+> pretty path'
#else
  mkSomeFullPath
    :: FilePath.FilePath
    -> m (Either (FullPath 'File) (FullPath 'Dir))
  mkSomeFullPath path = do
    exists <- liftBase $ Posix.fileExist path
    unless exists $
      throwErrorWithCallStack $ "Path does not exist:" <+> pretty path
    status <- liftBase $ Posix.getSymbolicLinkStatus path
    path'  <- liftBase $ T.pack <$> Directory.makeAbsolute path
    if | Posix.isRegularFile  status -> pure $! Left  $! FullPath path'
       | Posix.isDirectory    status -> pure $! Right $! FullPath path'
       | Posix.isSymbolicLink status -> do
         status' <- liftBase $ try $ Posix.getFileStatus path
         case status' of
           Left (e :: IOException) -> throwErrorWithCallStack $
             "Error while getting status of path" <+> PP.squotes (pretty path) <+> ":" ## ppShow e
           Right status''
             | isRegularFile  status'' -> pure $! Left  $! FullPath path'
             | isDirectory    status'' -> pure $! Right $! FullPath path'
             | otherwise               -> throwErrorWithCallStack $
               "Symbolic link" <+> PP.squotes (pretty path) <+> "refers to neither a file nor a directory."
       | otherwise            -> throwErrorWithCallStack $
          "Path" <+> PP.squotes (pretty path) <+> "refers to neither a file, directory or a symbolic link."
#endif

instance MkSomeFullPath FilePath.FilePath m => MkSomeFullPath Text m where
  {-# INLINE mkSomeFullPath #-}
  mkSomeFullPath = mkSomeFullPath . T.unpack

instance MkSomeFullPath Text m => MkSomeFullPath PathFragment m where
  {-# INLINE mkSomeFullPath #-}
  mkSomeFullPath = mkSomeFullPath . unPathFragment

class MkFullPath a typ m where
  mkFullPath :: a -> m (FullPath typ)

instance (MkSomeFullPath FilePath.FilePath m, MonadError ErrorMessage m) => MkFullPath FilePath.FilePath 'File m where
  {-# INLINE mkFullPath #-}
  mkFullPath path = do
    somePath <- mkSomeFullPath path
    case somePath of
      Left  x -> pure x
      Right _ -> throwErrorWithCallStack $ "Path does not refer to a file:" <+> pretty path

instance (MkSomeFullPath FilePath.FilePath m, MonadError ErrorMessage m) => MkFullPath FilePath.FilePath 'Dir m where
  {-# INLINE mkFullPath #-}
  mkFullPath path = do
    somePath <- mkSomeFullPath path
    case somePath of
      Left  _ -> throwErrorWithCallStack $ "Path does not refer to a directory:" <+> pretty path
      Right x -> pure x

instance MkFullPath FilePath.FilePath a m => MkFullPath Text a m where
  {-# INLINE mkFullPath #-}
  mkFullPath = mkFullPath . T.unpack

instance MkFullPath FilePath.FilePath a m => MkFullPath PathFragment a m where
  {-# INLINE mkFullPath #-}
  mkFullPath = mkFullPath . unPathFragment

{-# INLINE doesFileExist #-}
doesFileExist :: MonadBase IO m => FullPath 'File -> m Bool
doesFileExist =
  liftBase . Directory.doesFileExist . T.unpack . unFullPath

{-# INLINE doesDirectoryExist #-}
doesDirectoryExist :: MonadBase IO m => FullPath 'Dir -> m Bool
doesDirectoryExist =
  liftBase . Directory.doesDirectoryExist . T.unpack . unFullPath

{-# INLINE listDirectory #-}
listDirectory
  :: MonadBase IO m
  => FullPath 'Dir
  -> m ([FullPath 'File], [FullPath 'Dir])
listDirectory (FullPath path) = do
  (dirs, files) <- liftBase $
    partitionIO Directory.doesDirectoryExist . map (path' FilePath.</>) =<< Directory.listDirectory path'
  pure $ coerce (map T.pack files, map T.pack dirs)
  where
    path' = T.unpack path

{-# INLINE getModificationTime #-}
getModificationTime :: MonadBase IO m => FullPath 'File -> m UTCTime
getModificationTime =
  liftBase . Directory.getModificationTime . T.unpack . unFullPath

{-# INLINE splitDirectories #-}
splitDirectories :: FullPath 'File -> ([BaseName 'Dir], BaseName 'File)
splitDirectories p = (map BaseName $ init ps, BaseName $ last ps)
  where
    ps = map (PathFragment . T.pack) . FilePath.splitDirectories . T.unpack . unFullPath $ p

{-# INLINE makeRelativeText #-}
makeRelativeText :: Text -> Text -> Text
makeRelativeText x y = T.pack $ FilePath.makeRelative (T.unpack x) (T.unpack y)

newtype PathJoin = PathJoin { unPathJoin :: Text }

instance Semigroup PathJoin where
  {-# INLINE (<>) #-}
  (<>) = coerce joinPath

mkPathFragment :: NonEmpty Text -> PathFragment
mkPathFragment = PathFragment . unPathJoin . foldMap1 PathJoin

{-# INLINE mkSinglePathFragment #-}
mkSinglePathFragment :: Text -> PathFragment
mkSinglePathFragment = PathFragment

class JoinPaths a b c | a b -> c where
  (</>) :: a -> b -> c

  {-# INLINE (</>) #-}
  default (</>) :: (Coercible a Text, Coercible b Text, Coercible c Text) => a -> b -> c
  (</>) = coerce joinPath

-- instance JoinPaths (FullPath 'Dir) PathFragment   (FullPath 'Dir)
instance JoinPaths (FullPath 'Dir) (BaseName typ) (FullPath typ)
-- -- instance JoinPaths PathFragment FullPath     PathFragment
instance JoinPaths PathFragment    PathFragment PathFragment
-- instance JoinPaths PathFragment    BaseName     PathFragment
--  -- instance JoinPaths BaseName     FullPath     PathFragment
-- instance JoinPaths BaseName        PathFragment PathFragment
-- instance JoinPaths BaseName        BaseName     PathFragment

class Contains a where
  isInfixOf :: Text -> a -> Bool
  {-# INLINE isInfixOf #-}
  default isInfixOf :: Coercible a Text => Text -> a -> Bool
  isInfixOf = coerce T.isInfixOf

instance Contains (FullPath a)
instance Contains PathFragment
instance Contains (BaseName a)

{-# INLINE mkExtension #-}
mkExtension :: Text -> Extension
mkExtension = Extension

class AddExtension a where
  (<.>) :: a -> Extension -> a
  {-# INLINE (<.>) #-}
  default (<.>) :: Coercible a Text => a -> Extension -> a
  (<.>) = coerce addExt

instance AddExtension (FullPath 'File)
instance AddExtension PathFragment
instance AddExtension (BaseName 'File)

class DropExtensions a where
  dropExtensions :: a -> a
  {-# INLINE dropExtensions #-}
  default dropExtensions :: Coercible a Text => a -> a
  dropExtensions = coerce dropExts

instance DropExtensions (FullPath 'File)
instance DropExtensions PathFragment
instance DropExtensions (BaseName 'File)

class TakeFileName a typ | a -> typ where
  takeFileName :: a -> BaseName typ
  {-# INLINE takeFileName #-}
  default takeFileName :: Coercible a Text => a -> BaseName typ
  takeFileName = coerce getFileName

instance TakeFileName (FullPath 'File) 'File
instance TakeFileName (FullPath 'Dir) 'Dir
instance TakeFileName PathFragment 'File

class TakeDirectory a where
  takeDirectory :: a -> FullPath 'Dir
  {-# INLINE takeDirectory #-}
  default takeDirectory :: Coercible a Text => a -> FullPath 'Dir
  takeDirectory = coerce getDirectory

instance TakeDirectory (FullPath a)
instance TakeDirectory PathFragment

class TakeExtension a where
  takeExtension :: a -> Extension
  {-# INLINE takeExtension #-}
  default takeExtension :: Coercible a Text => a -> Extension
  takeExtension = coerce getExtension

instance TakeExtension (FullPath 'File)
instance TakeExtension PathFragment

class MakeRelative a b c | a b -> c where
  makeRelative
    :: a -- ^ Root to make relative to
    -> b -- ^ What to transform
    -> c
  {-# INLINE makeRelative #-}
  default makeRelative
    :: (Coercible a Text, Coercible b Text, Coercible c Text)
    => a -> b -> c
  makeRelative = coerce makeRelativeText

instance MakeRelative (FullPath 'Dir) (FullPath a) PathFragment
-- instance MakeRelative (FullPath 'Dir) PathFragment PathFragment
-- instance MakeRelative PathFragment    PathFragment PathFragment

-- Internals

{-# INLINE getFileName #-}
getFileName :: Text -> Text
getFileName = T.pack . FilePath.takeFileName . T.unpack

{-# INLINE getDirectory #-}
getDirectory :: Text -> Text
getDirectory =
  -- T.pack . FilePath.takeDirectory . T.unpack
  T.dropEnd 1 . T.dropWhileEnd (not . FilePath.isPathSeparator)

{-# INLINE getExtension #-}
getExtension :: Text -> Text
getExtension =
  T.cons FilePath.extSeparator . T.takeWhileEnd (/= FilePath.extSeparator)
-- T.pack . FilePath.takeExtension . T.unpack

{-# INLINE joinPath #-}
joinPath :: Text -> Text -> Text
joinPath x y = x <> T.singleton FilePath.pathSeparator <> y

{-# INLINE addExt #-}
addExt :: Text -> Text -> Text
addExt path ext = path <> extSeparator Semigroup.<> ext

{-# INLINE dropExts #-}
dropExts :: Text -> Text
dropExts =
  T.pack . FilePath.dropExtensions . T.unpack
-- path = case T.splitOn extSeparator path of
--   []    -> path
--   p : _ -> p

{-# INLINE extSeparator #-}
extSeparator :: Text
extSeparator = T.singleton FilePath.extSeparator

{-# INLINE fullPathAsUtf8 #-}
fullPathAsUtf8 :: FullPath typ -> BS.ByteString
fullPathAsUtf8 = TE.encodeUtf8 . unFullPath

{-# INLINE toFilePath #-}
toFilePath :: FullPath typ -> FilePath.FilePath
toFilePath = T.unpack . unFullPath
