----------------------------------------------------------------------------
-- |
-- Module      :  Data.Path.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}

#ifdef mingw32_HOST_OS
#define WINDOWS 1
#endif

module Data.Path.Internal
  ( FileType(..)
  , FullPath(..)
  , PathFragment(..)
  , Extension(..)
  , BaseName(..)
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Hashable
import Data.String
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Ext

#ifdef WINDOWS
import Data.Coerce
import Data.Function (on)
import qualified Data.Text as T
#endif

-- | A type-level label to distinguish directories and files.
data FileType = Dir | File

-- | Absolute path, canonicalised and normalised. Provides strictest invariants
-- but must be created in derivatives of IO monad.
-- Invariant: does not end with \/.
newtype FullPath (typ :: FileType) = FullPath { unFullPath :: Text }
  deriving (Show, Pretty, IsString, NFData, Hashable, Binary)

#ifdef WINDOWS
instance Eq (FullPath a) where
  {-# INLINE (==) #-}
  (==) = coerce ((==) `on` T.toCaseFold)

instance Ord (FullPath a) where
  {-# INLINE compare #-}
  compare = coerce (compare `on` T.toCaseFold)
#else
deriving instance Eq  (FullPath a)
deriving instance Ord (FullPath a)
#endif

-- | Path fragment, possibly with some directories but without etxension.
-- Invariant: does not start with \/, does not end with \/.
newtype PathFragment = PathFragment { unPathFragment :: Text }
  deriving (Show, Pretty, IsString, NFData, Hashable, Binary)

#ifdef WINDOWS
instance Eq PathFragment where
  {-# INLINE (==) #-}
  (==) = coerce ((==) `on` T.toCaseFold)

instance Ord PathFragment where
  {-# INLINE compare #-}
  compare = coerce (compare `on` T.toCaseFold)
#else
deriving instance Eq  PathFragment
deriving instance Ord PathFragment
#endif

-- | E.g. “.hs”.
newtype Extension = Extension { unExtension :: Text }
  deriving (Show, IsString, NFData, Hashable, Binary)

#ifdef WINDOWS
instance Eq Extension where
  {-# INLINE (==) #-}
  (==) = coerce ((==) `on` T.toCaseFold)

instance Ord Extension where
  {-# INLINE compare #-}
  compare = coerce (compare `on` T.toCaseFold)
#else
deriving instance Eq  Extension
deriving instance Ord Extension
#endif

-- | File basename without directory but with extension.
newtype BaseName (typ :: FileType) = BaseName { unBaseName :: PathFragment }
  deriving (Show, Pretty, IsString, NFData, Hashable, Binary)

#ifdef WINDOWS
instance Eq (BaseName typ) where
  {-# INLINE (==) #-}
  (==) = coerce ((==) `on` T.toCaseFold)

instance Ord (BaseName typ) where
  {-# INLINE compare #-}
  compare = coerce (compare `on` T.toCaseFold)
#else
deriving instance Eq  (BaseName typ)
deriving instance Ord (BaseName typ)
#endif

