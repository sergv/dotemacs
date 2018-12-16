----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithExportList
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ModuleWithExportList (foo, bar) where

foo :: a -> a
foo = baz

bar :: a -> a
bar = baz

baz :: a -> a
baz x = x
