----------------------------------------------------------------------------
-- |
-- Module      :  ImportsBViaReexports
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ImportsBViaReexports (quuxB, module R) where

import Reexports as R
  (FooB(..), BarB(..), BazBP, quuxB, FrobBP, QuuxB(..), QuuxBP, derivedB)
