----------------------------------------------------------------------------
-- |
-- Module      :  BWithExportList
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday,  4 October 2016
----------------------------------------------------------------------------

module BWithExportList (TB(..), g) where

import {-# SOURCE #-} AWithExportList( TA(..) )


data TB = MkTB !Int

g :: TA -> TB
g (MkTA x) = MkTB x
