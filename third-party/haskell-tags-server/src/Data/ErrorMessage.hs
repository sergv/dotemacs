----------------------------------------------------------------------------
-- |
-- Module      :  Data.ErrorMessage
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :   5 June 2017
----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.ErrorMessage (ErrorMessage(..)) where

import Control.Exception

-- import Data.String
import Data.Text.Prettyprint.Doc.Ext
import Data.Void (Void, vacuous)
import GHC.Stack.Ext

data ErrorMessage = ErrorMessage
  { errorMessageBody      :: Doc Void
  , errorMessageBacktrace :: CallStack
  } deriving (Show)

instance Exception ErrorMessage

instance Pretty ErrorMessage where
  pretty ErrorMessage{errorMessageBody, errorMessageBacktrace} =
    case getCallStack errorMessageBacktrace of
      []    -> errorMessageBody'
      _ : _ ->
        errorMessageBody' ##
          "Backtrace:" ## ppCallStack errorMessageBacktrace
    where
      errorMessageBody' = vacuous errorMessageBody
