----------------------------------------------------------------------------
-- |
-- Module      :  PortPool
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 21 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module PortPool
  ( PortPool
  , newPortPool
  , getPort
  , restorePort
  , withPort
  ) where

import Control.Concurrent
import Control.Monad.Base
import Control.Monad.Catch

import Network.Socket (PortNumber)

data PortPool = PortPool
  { portPoolPorts    :: Chan PortNumber
  , portPoolCapacity :: !Int
  }

newPortPool :: MonadBase IO m => Int -> PortNumber -> m PortPool
newPortPool capacity startPort
  | startPort < 0 = error "Cannot create port pool with negative start port"
  | capacity  < 0 = error "Cannot create port pool with negative capacity"
  | otherwise     = do
    portsChan <- liftBase newChan
    liftBase $ writeList2Chan portsChan ports
    pure PortPool
      { portPoolPorts    = portsChan
      , portPoolCapacity = capacity
      }
  where
    ports :: [PortNumber]
    ports = takeWhile ((<= maxPort) . fromIntegral)
          $ take capacity [startPort..]
    maxPort :: Int
    maxPort = 65535

getPort :: MonadBase IO m => PortPool -> m PortNumber
getPort = liftBase . readChan . portPoolPorts

restorePort :: MonadBase IO m => PortPool -> PortNumber -> m ()
restorePort pool = liftBase . writeChan (portPoolPorts pool)

withPort
  :: (MonadMask m, MonadBase IO m)
  => PortPool -> (PortNumber -> m a) -> m a
withPort pool = bracket (getPort pool) (restorePort pool)
