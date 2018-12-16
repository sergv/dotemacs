----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithMultilineExportList
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ModuleWithMultilineExportList
  ( foo2
  , bar2
  )
where

foo2 :: a -> a
foo2 = baz2

bar2 :: a -> a
bar2 = baz2

baz2 :: a -> a
baz2 x = x
