----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.EitherCPS
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- A continuation-based error monad.
----------------------------------------------------------------------------

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.EitherCPS
  ( EitherCPST
  , runEitherCPST
  ) where

import Control.Applicative
#if MIN_VERSION_mtl(2, 2, 0)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif
import Control.Monad.Reader
import Control.Monad.State
import Data.String

newtype EitherCPST e m a = EitherCPST
  { runEitherCPST :: forall r. (e -> m r) -> (a -> m r) -> m r }
  deriving (Functor)

instance Applicative (EitherCPST e m) where
  {-# INLINE pure #-}
  pure x = EitherCPST $ \_ sk -> sk x
  {-# INLINE (<*>) #-}
  EitherCPST f <*> EitherCPST g = EitherCPST $
      \ek sk -> f ek (\f' -> g ek (sk . f'))

instance IsString e => Alternative (EitherCPST e m) where
  {-# INLINE empty #-}
  empty = throwError "zero"
  {-# INLINE (<|>) #-}
  EitherCPST f <|> EitherCPST f' = EitherCPST $ \ek sk -> f (\_ -> f' ek sk) sk

instance Monad (EitherCPST e m) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  EitherCPST f >>= m = EitherCPST $ \ek sk ->
    f ek (\x -> runEitherCPST (m x) ek sk)

instance MonadTrans (EitherCPST e) where
  {-# INLINE lift #-}
  lift action = EitherCPST $ \_ sk -> action >>= sk

instance MonadError e (EitherCPST e m) where
  {-# INLINE throwError #-}
  throwError e = EitherCPST $ \ek _ -> ek e
  {-# INLINE catchError #-}
  catchError (EitherCPST f) handler = EitherCPST $ \ek sk ->
    f (\e -> runEitherCPST (handler e) ek sk) sk

instance MonadState s m => MonadState s (EitherCPST e m) where
  {-# INLINE get #-}
  get = EitherCPST $ \_ sk -> get >>= sk
  {-# INLINE put #-}
  put x = EitherCPST $ \_ sk -> put x >>= sk

instance MonadReader r m => MonadReader r (EitherCPST e m) where
  {-# INLINE ask    #-}
  ask = EitherCPST $ \_ sk -> ask >>= sk
  {-# INLINE local  #-}
  local f (EitherCPST g) = EitherCPST $ \fk sk -> local f $ g fk sk
  {-# INLINE reader #-}
  reader f = EitherCPST $ \_ sk -> reader f >>= sk
