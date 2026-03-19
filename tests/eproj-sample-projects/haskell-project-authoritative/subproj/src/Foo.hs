----------------------------------------------------------------------------
-- |
-- Module      :  Foo
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Foo where



-- Entries in this file are offset so that we can distinguish actual tags by
-- their lines.

foo :: Int -> Int
foo = id

bar :: Double
bar = 3.1415

-- This 'FooConstructor' differs from constructors in other projects.
-- Aim of the test is to check that authoritative source will override
-- even if text differs. That's what authoritativeness means.
data Foo = FooConstructor Int Int
