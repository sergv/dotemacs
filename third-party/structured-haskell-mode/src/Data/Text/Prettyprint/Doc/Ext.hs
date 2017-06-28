----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Prettyprint.Doc.Ext
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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.Text.Prettyprint.Doc.Ext
  ( putDocLn
  , displayDoc
  , displayDocString
  , show'
  , showLazy
  , showDoc
  , docFromString
  , docFromByteString
  , docFromText
  , ppList
  , ppListDoc
  , ppList'
  , ppAlist
  , ppDict
  , ppFoldableWithHeader
  , ppMap
  , ppNE
  , ppSet
  , MapEntry(..)
  , (##)
  , ppTrace
  , ppCallStack
  , module PP
  ) where

import Control.Monad.Base
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Prettyprint.Doc (Pretty(..), Doc, (<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as Render
import GHC.Stack

import Debug.Trace

putDocLn :: MonadBase IO m => Doc a -> m ()
putDocLn = liftBase . TLIO.putStrLn . displayDoc

displayDoc :: Doc a -> TL.Text
displayDoc = Render.renderLazy . PP.layoutSmart layoutOptions
  where
    layoutOptions = PP.defaultLayoutOptions
      { PP.layoutPageWidth = PP.AvailablePerLine 80 0.9
      }

displayDocString :: Doc a -> String
displayDocString = TL.unpack . displayDoc

show' :: Show a => a -> T.Text
show' = T.pack . show

showLazy :: Show a => a -> TL.Text
showLazy = TL.pack . show

showDoc :: Show a => a -> Doc a
showDoc = docFromString . show

docFromString :: String -> Doc a
docFromString = PP.pretty

docFromByteString :: UTF8.ByteString -> Doc a
docFromByteString = PP.pretty . TLE.decodeUtf8With TEE.lenientDecode

docFromText :: T.Text -> Doc a
docFromText = PP.pretty

ppList :: Pretty a => [a] -> Doc b
ppList = ppListDoc . map pretty

ppListDoc :: [Doc a] -> Doc a
ppListDoc = ppList' PP.lbracket PP.rbracket

ppList'
  :: forall a f. (Functor f, Foldable f)
  => Doc a -> Doc a -> f (Doc a) -> Doc a
ppList' left right xs =
  case toList xs of
    []   -> left <> right
    [y]  -> PP.flatAlt (left PP.<+> PP.align y <> PP.line <> right) (left <> y <> right)
    y:ys ->
      PP.align $
      PP.group $ PP.vcat
        [ PP.flatAlt (left PP.<+> PP.align y) (left <> PP.align y)
        , PP.vcat (fmap (\x -> separator PP.<+> PP.align x) ys) -- <> PP.line <> right
        , right
        ]
  where
    separator = ","

ppAlist :: forall a. [MapEntry TL.Text (Doc a)] -> Doc a
ppAlist entries = ppList' PP.lbrace PP.rbrace $ map ppMapEntry entries'
  where
    entries' :: [MapEntry (Doc a) (Doc a)]
    entries' = map (\(k :-> v) -> PP.fillBreak maxWidth (PP.pretty k) :-> v) entries
    maxWidth :: Int
    maxWidth = fromIntegral $ maximum $ map (\(k :-> _) -> TL.length k) entries

ppDict :: Doc a -> [MapEntry TL.Text (Doc a)] -> Doc a
ppDict header entries = header ## ppAlist entries

(##) :: Doc a -> Doc a -> Doc a
(##) header body =
  PP.nest 2 $ header <> PP.line <> body

ppFoldableWithHeader :: (Foldable f, Pretty b) => Doc a -> f b -> Doc a
ppFoldableWithHeader header entries =
  PP.nest 2 $ PP.vsep
    [ header
    , PP.vsep (map (("-" PP.<+>) . PP.align . pretty) $ toList entries)
    ]

ppMap :: (Pretty a, Pretty b) => Map a b -> Doc c
ppMap = ppAlist' . M.toList

ppAlist' :: (Pretty k, Pretty v) => [(k, v)] -> Doc c
ppAlist' = ppList' PP.lbrace PP.rbrace . map (pretty . uncurry (:->))

ppNE :: Pretty a => NonEmpty a -> Doc b
ppNE = ppList' PP.lbracket PP.rbracket . map pretty . toList

ppSet :: Pretty a => Set a -> Doc b
ppSet = ppList' PP.lbrace PP.rbrace . map pretty . toList

infix 0 :->

data MapEntry k v = k :-> v
  deriving (Eq, Ord, Show)

instance (Pretty k, Pretty v) => Pretty (MapEntry k v) where
  pretty (x :-> y) = ppMapEntry (pretty x :-> pretty y)

ppMapEntry :: MapEntry (Doc a) (Doc a) -> Doc a
ppMapEntry (x :-> y) =
  PP.group $ x <+> "->" ## PP.align y

ppTrace :: Bool -> Doc a -> b -> b
ppTrace False _   = id
ppTrace True  msg = trace (displayDocString msg)

ppCallStack :: CallStack -> Doc a
ppCallStack =
  PP.vcat .
  map (\(name, loc) -> PP.hcat
        [ docFromString (srcLocModule loc)
        , "."
        , docFromString name
        , ":"
        , pretty (srcLocStartLine loc)
        , ":"
        , pretty (srcLocStartCol loc)
        ]
        ) .
  getCallStack
