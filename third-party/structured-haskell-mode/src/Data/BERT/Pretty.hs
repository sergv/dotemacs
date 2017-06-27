----------------------------------------------------------------------------
-- |
-- Module      :  Data.BERT.Pretty
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

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.BERT.Pretty (ppBert) where

import Data.BERT
import Data.Text.Prettyprint.Doc
import Data.Void

import Data.Text.Prettyprint.Doc.Ext

ppBert :: Term -> Doc a
ppBert = fmap absurd . go
  where
    go :: Term -> Doc Void
    go = \case
      IntTerm n            -> pretty n
      FloatTerm x          -> pretty x
      AtomTerm str         -> pretty str
      TupleTerm ts         -> ppList' lparen rparen $ map go ts
      BytelistTerm bstr    -> docFromByteString bstr
      ListTerm ts          -> ppList' lbracket rbracket $ map (group . go) ts
      BinaryTerm bstr      -> dquotes $ docFromByteString bstr
      BigintTerm n         -> pretty n
      BigbigintTerm n      -> pretty n
      NilTerm              -> "Nil"
      BoolTerm b           -> pretty b
      DictionaryTerm assoc -> ppList' lbrace rbrace $
                              map (\(x, y) -> nest 2 $ go x <+> "->" <> line <> align (group $ go y)) assoc
      TimeTerm time        -> pretty $ show time
      RegexTerm re opts    -> pretty re <> pretty opts

