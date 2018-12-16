----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithImportsAndHiding
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ModuleWithImportsAndHiding where

import Imported1
import Imported2 hiding (bar2)

baz :: a -> a
baz x = x
