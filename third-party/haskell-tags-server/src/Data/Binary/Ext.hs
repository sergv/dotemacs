----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Ext
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Binary.Ext
  ( module Data.Binary
  ) where

import Data.Binary
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Generics

deriving instance Generic Day
instance Binary Day

instance Binary DiffTime where
  put = put . diffTimeToPicoseconds
  get = picosecondsToDiffTime <$> get

deriving instance Generic UTCTime
instance Binary UTCTime
