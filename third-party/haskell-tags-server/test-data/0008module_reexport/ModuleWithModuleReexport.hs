----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithModuleReexport
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ModuleWithModuleReexport
  ( module Module1, module ModuleWithModuleReexport )
where

import Module1

test :: a -> b -> a
test x _ = x
