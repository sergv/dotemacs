----------------------------------------------------------------------------
-- |
-- Module      :  Foo
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE PackageImports #-}

module Foo (foofoo, bar) where

import "other-module" Dependency (bar)

foofoo :: Int -> Int
foofoo x = x
