----------------------------------------------------------------------------
-- |
-- Module      :  AWithExportList
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday,  4 October 2016
----------------------------------------------------------------------------

module AWithExportList (TA(MkTA), f) where

import BWithExportList( TB(..), g )

newtype TA = MkTA Int

f :: TB -> TA
f (MkTB x) = MkTA x
