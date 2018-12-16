----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithModuleReexportViaAlias
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday,  2 November 2016
----------------------------------------------------------------------------

module ModuleWithModuleReexportViaAlias (module Mod) where

import Module2 as Mod
