----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithQualifiedModuleReexport
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

-- Does not export any names, see ‘https://ro-che.info/articles/2012-12-25-haskell-module-system-p1’ for details
module ModuleWithQualifiedModuleReexport
  ( module MyModule )
where

import qualified Module3 as MyModule

