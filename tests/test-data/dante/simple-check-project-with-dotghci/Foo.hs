-- |
-- Module:     Foo
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE CPP #-}

module Foo (foo) where

#if defined(THROW_ERROR)
#error "The throw-error flag is defined"
#endif

foo :: Int -> Int
foo = id
