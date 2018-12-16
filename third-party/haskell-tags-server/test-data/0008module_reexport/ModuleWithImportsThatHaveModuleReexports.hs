----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithImportsThatHaveModuleReexports
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ModuleWithImportsThatHaveModuleReexports where

import ModuleWithModuleReexport
import ModuleWithModuleReexportViaAlias
-- this brings in no new names because that's what GHC does
import ModuleWithQualifiedModuleReexport

