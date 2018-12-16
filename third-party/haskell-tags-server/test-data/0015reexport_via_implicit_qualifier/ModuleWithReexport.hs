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
  ) where

import Source

lookup :: Int -> Int
lookup = id
