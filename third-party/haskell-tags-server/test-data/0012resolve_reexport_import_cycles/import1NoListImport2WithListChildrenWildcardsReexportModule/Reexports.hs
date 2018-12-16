----------------------------------------------------------------------------
-- |
-- Module      :  Reexports
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Reexports
  ( module BWildcardExportListWithChildren
  , module Public
  , module DSpecificExportListWithChildren
  , module ESpecificExportListWithChildrenPlusSome
  , module Reexports
  ) where

import ANoExportList as Public

import CausesImportCycle

import BWildcardExportListWithChildren
import CWildcardExportListWithChildrenPlusSome as Public

import DSpecificExportListWithChildren
import ESpecificExportListWithChildrenPlusSome

reexportsFunc :: Int -> Int
reexportsFunc x = x + 1

data ReexportType =
    ReexportC1 Int
  | ReexportC2
