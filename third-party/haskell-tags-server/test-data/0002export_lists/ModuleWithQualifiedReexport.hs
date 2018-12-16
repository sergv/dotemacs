----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithQualifiedReexport
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ModuleWithQualifiedReexport
  (foo2, Quux.bar2)
where

import ModuleWithMultilineExportList as Quux
