----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithMultilineImportList
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ModuleWithMultilineImportList where

import Imported1 ( foo
                 , bar
                 )
-- somewhat degenerate but still syntactically valid import
import
 Imported2
 ( foo2
 , bar2
 )

baz :: a -> a
baz x = x
