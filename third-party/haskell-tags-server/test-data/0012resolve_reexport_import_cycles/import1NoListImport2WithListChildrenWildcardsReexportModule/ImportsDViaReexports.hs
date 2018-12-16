----------------------------------------------------------------------------
-- |
-- Module      :  ImportsDViaReexports
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ImportsDViaReexports (quuxD, module R) where

import qualified Reexports as R
  (FooD(..), BarD(..), BazDP, quuxD, FrobDP, QuuxD(..), QuuxDP, derivedD)
