----------------------------------------------------------------------------
-- |
-- Module      :  B
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday,  4 October 2016
----------------------------------------------------------------------------

module B where

import {-# SOURCE #-} A( TA(..) )


data TB = MkTB !Int

g :: TA -> TB
g (MkTA x) = MkTB x
