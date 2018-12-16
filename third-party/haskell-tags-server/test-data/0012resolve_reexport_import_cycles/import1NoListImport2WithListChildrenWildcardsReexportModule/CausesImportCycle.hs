----------------------------------------------------------------------------
-- |
-- Module      :  CausesImportCycle
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module CausesImportCycle (foo) where

import ImportsAViaReexports
import ImportsBViaReexports
import ImportsCViaReexports
import ImportsDViaReexports
import ImportsEViaReexports

foo :: Int -> Int
foo = quuxA . quuxB . quuxC . quuxD . quuxE
