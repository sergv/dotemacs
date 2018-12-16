----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithExplicitExport
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ModuleWithExplicitExport
  ( Foo2(Bar2, getBar2)
  )
where

data Foo2 = Bar2 { getBar2 :: Int }
          | Baz2 { getBaz2 :: Int }
  deriving (Eq)
