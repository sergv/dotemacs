-- |
-- Module:     Foo
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Foo.Bar (foo) where

import Baz.Quux
import Dep

foo :: Int -> Int
foo = dep . bar . bar
