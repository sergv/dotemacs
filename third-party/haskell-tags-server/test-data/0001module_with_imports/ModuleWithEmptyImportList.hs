----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithEmptyImportList
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ModuleWithEmptyImportList where

import Imported1 ()
import Imported2 (foo2)

baz :: a -> a
baz x = x
