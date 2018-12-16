----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithQualifiedImport
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ModuleWithQualifiedImport where

import qualified Imported1 as Imp

baz :: a -> a
baz x = x
