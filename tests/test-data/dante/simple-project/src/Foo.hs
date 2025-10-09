-- |
-- Module:     Foo
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Foo (foo) where

import Bar.Baz

foo :: a -> a
foo = baz
