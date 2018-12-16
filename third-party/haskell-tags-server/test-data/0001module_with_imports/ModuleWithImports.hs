----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithImports
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE TypeOperators #-}

module ModuleWithImports where

import Imported1
import Imported2 (foo2, ($$*), (:$$*:)((:$$$*:)))

baz :: a -> a
baz x = x
