----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithEmptyExportList
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ModuleWithEmptyExportList () where

data Foo =
  Bar
    Baz

data Baz =
  Quux
    !Int
    String

frob :: Int -> Baz
frob x = Quux x "wat"
