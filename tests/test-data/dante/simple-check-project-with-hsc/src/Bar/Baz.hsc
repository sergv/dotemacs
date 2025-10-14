-- |
-- Module:     Bar.Baz
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE ForeignFunctionInterface #-}

module Bar.Baz (baz) where

#include <math.h>

type CDoubleTyp = #{type double}

foreign import ccall unsafe "math.h sin" sinFunc :: Double -> CDoubleTyp

baz :: Double -> CDoubleTyp
baz x = sinFunc x
