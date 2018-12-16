----------------------------------------------------------------------------
-- |
-- Module      :  Import1NoListImport2WithListChildrenWildcardsReexportModule.ImportsEViaReexports
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ImportsEViaReexports (module Reexports) where

import Reexports
  (FooE(..), BarE(..), quuxE, QuuxE(..), derivedE)
