----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Stack.Ext
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP             #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures  #-}

module GHC.Stack.Ext
  ( WithCallStack
  , module GHC.Stack
  ) where

import Data.Kind
import GHC.Stack

#ifdef DEVELOPMENT
type WithCallStack = HasCallStack
#else
type WithCallStack = (() :: Constraint)
#endif
