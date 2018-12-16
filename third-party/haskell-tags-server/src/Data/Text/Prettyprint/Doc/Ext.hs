----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Prettyprint.Doc.Ext
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 30 August 2016
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# OPTIONS_GHC -Wredundant-constraints          #-}
{-# OPTIONS_GHC -Wsimplifiable-class-constraints #-}

module Data.Text.Prettyprint.Doc.Ext
  ( Pretty(..)
  , (<+>)
  , show'
  , show''
  , ppKeyMapWith
  , ppSubkeyMapWith
  , ppNEMap
  , ppMonoidalMapWith
  , docFromByteString

  , module Data.Text.Prettyprint.Doc.Combinators
  , module Data.Text.Prettyprint.Doc.Generics
  , module Data.Text.Prettyprint.Doc.Show
  ) where

import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Foldable (toList)
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Prettyprint.Doc (Pretty(..), Doc, (<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Combinators
import Data.Text.Prettyprint.Doc.Generics
import Data.Text.Prettyprint.Doc.Show

import Data.KeyMap (KeyMap)
import qualified Data.KeyMap as KM
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.MonoidalMap (MonoidalMap)
import qualified Data.MonoidalMap as MM
import Data.SubkeyMap (SubkeyMap)
import qualified Data.SubkeyMap as SubkeyMap

docFromByteString :: UTF8.ByteString -> Doc ann
docFromByteString = PP.pretty . TLE.decodeUtf8With TEE.lenientDecode

ppKeyMapWith
  :: Foldable f
  => (KM.Key a -> Doc ann)
  -> (a -> Doc ann)
  -> KeyMap f a
  -> Doc ann
ppKeyMapWith ppKey ppVal = ppAssocListWith ppKey (ppListWith ppVal . toList) . KM.toList

ppSubkeyMapWith
  :: (k -> Doc ann)
  -> (SubkeyMap.Subkey k -> Doc ann)
  -> (v -> Doc ann)
  -> SubkeyMap k v
  -> Doc ann
ppSubkeyMapWith ppKey ppSubKey ppVal sm = ppDictHeader "SubkeyMap"
  [ "MainEntries" :-> ppAssocListWith ppKey ppVal (SubkeyMap.toList sm)
  , "SubEntries"  :-> ppAssocListWith ppSubKey (ppSetWith ppKey) $ SubkeyMap.toSubkeyKeyList sm
  ]

ppNEMap :: (Pretty k, Pretty v) => NonEmptyMap k v -> Doc ann
ppNEMap = ppAssocList . toList . NEMap.toNonEmpty

ppMonoidalMapWith
  :: (k -> Doc ann)
  -> (v -> Doc ann)
  -> MonoidalMap k v
  -> Doc ann
ppMonoidalMapWith k v = ppMapWith k v . MM.unMonoidalMap

show' :: Show a => a -> T.Text
show' = T.pack . show

show'' :: Show a => a -> TL.Text
show'' = TL.pack . show
