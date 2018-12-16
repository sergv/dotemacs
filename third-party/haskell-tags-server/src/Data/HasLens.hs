----------------------------------------------------------------------------
-- |
-- Module      :  Data.HasLens
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  19 June 2017
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wredundant-constraints          #-}
{-# OPTIONS_GHC -Wsimplifiable-class-constraints #-}

module Data.HasLens
  ( HasLens(..)
  ) where

import Lens.Micro

class HasLens s a where
  fieldLens :: Lens' s a

instance HasLens (a, b) a where
  fieldLens = _1

instance HasLens (a, b) b where
  fieldLens = _2
