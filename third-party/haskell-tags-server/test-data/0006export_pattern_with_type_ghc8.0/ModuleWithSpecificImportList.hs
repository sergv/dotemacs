----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithSpecificImportList
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday, 20 October 2016
----------------------------------------------------------------------------

module ModuleWithSpecificImportList where

import ModuleThatExportsPattern (FooTyp(Foo, Bar, Foo', Baz'))
