----------------------------------------------------------------------------
-- |
-- Module      :  Data.ErrorMessage
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

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.ErrorMessage
  ( ErrorMessage(..)
  , formatErrorMessage
  ) where

import Data.Semigroup
import qualified Data.Text.Lazy as TL
import GHC.Stack

data ErrorMessage = ErrorMessage
  { emMessage   :: TL.Text
  , emCallStack :: CallStack
  }

formatErrorMessage :: ErrorMessage -> TL.Text
formatErrorMessage ErrorMessage{emMessage, emCallStack} =
  TL.unlines $
    emMessage :
    "Backtrace:" :
    map (("  " <>) . ppEntry) (getCallStack emCallStack)
  where
    ppEntry :: (String, SrcLoc) -> TL.Text
    ppEntry (name, loc) = TL.concat
      [ TL.pack $ srcLocModule loc
      , "."
      , TL.pack name
      , ":"
      , show' $ srcLocStartLine loc
      , ":"
      , show' $ srcLocStartCol loc
      ]

show' :: Show a => a -> TL.Text
show' = TL.pack . show

