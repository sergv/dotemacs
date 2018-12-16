----------------------------------------------------------------------------
-- |
-- Module      :  ModuleThatExportsPattern
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday, 20 October 2016
----------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}

module ModuleThatExportsPattern (FooTyp(.., Foo', Baz')) where

data FooTyp =
    Foo Int Int
  | Bar [Int]

pattern Foo' y = Foo 2 y
pattern Baz' x y z = Bar [x, y,  z]
