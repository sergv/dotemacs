----------------------------------------------------------------------------
-- |
-- Module      :  B
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Sunday, 23 October 2016
----------------------------------------------------------------------------

module B (FooTyp(..), BarTyp, module A) where

import A (foo, bar)

data FooTyp = Foo Int

data BarTyp = Bar Double

data BazTyp = Baz String
