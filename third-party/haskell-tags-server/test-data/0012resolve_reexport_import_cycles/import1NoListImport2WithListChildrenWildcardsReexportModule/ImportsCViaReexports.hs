----------------------------------------------------------------------------
-- |
-- Module      :  ImportsCViaReexports
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module ImportsCViaReexports (quuxC, module R) where

import qualified Reexports as R
  (FooC(..), BarC(..), quuxC, QuuxC(..), derivedC)
