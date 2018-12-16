----------------------------------------------------------------------------
-- |
-- Module      :  MainModule
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 20 September 2016
----------------------------------------------------------------------------

module MainModule where

import ModuleWithTypeclass

foo :: TestFam Int -> Int
foo = unIntBox
