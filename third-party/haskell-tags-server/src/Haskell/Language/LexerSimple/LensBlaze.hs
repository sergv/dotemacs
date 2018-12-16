----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.LexerSimple.LensBlaze
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.LexerSimple.LensBlaze
  ( Lens
  , Lens'
  , lens
  , view
  , over
  , set
  , int16L
  , int16L'
  ) where

import Control.Applicative

import Data.Bits
import Data.Functor.Identity
import Data.Int

type Lens' s a = Lens s s a a
type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

{-# INLINE lens #-}
lens :: (s -> a) -> (b -> s -> t) -> Lens s t a b
lens access write = \f s -> (\b -> write b s) <$> f (access s)

{-# INLINE view #-}
view :: Lens s t a b -> s -> a
view l = getConst . l Const

{-# INLINE set #-}
set :: Lens s t a b -> b -> s -> t
set l = over l . const

{-# INLINE over #-}
over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

{-# INLINE int16L #-}
int16L :: (Bits b, Integral b) => Int -> Lens' b Int16
int16L n = int16L' n 0xffff

{-# INLINE int16L' #-}
int16L' :: forall a b. (Integral a, Bits b, Integral b) => Int -> b -> Lens' b a
int16L' n !mask = \f x ->
  (\x' -> (fromIntegral x' `unsafeShiftL` n) .|. (x .&. reverseMask)) <$> f (fromIntegral ((x `unsafeShiftR` n) .&. mask :: b))
  where
    reverseMask :: b
    !reverseMask = complement $ mask `unsafeShiftL` n
