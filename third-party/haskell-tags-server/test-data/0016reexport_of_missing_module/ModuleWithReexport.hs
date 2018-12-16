----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithReexport
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ModuleWithReexport
  ( Source.foo
  , Source.Bar
  , ModuleWithReexport.lookup
  , Import.foo2
  , Source3.Quux
  ) where

import Source
import {-# SOURCE #-} qualified Source2 as Import
import Source3

lookup :: Int -> Int
lookup = id
