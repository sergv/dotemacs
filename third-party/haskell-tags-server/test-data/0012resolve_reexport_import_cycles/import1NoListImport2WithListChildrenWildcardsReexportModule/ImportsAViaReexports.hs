----------------------------------------------------------------------------
-- |
-- Module      :  ImportsAViaReexports
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ImportsAViaReexports (module Reexports) where

import Reexports
  (FooA(..), BarA(..), BazAP, quuxA, FrobAP, QuuxA(..))
