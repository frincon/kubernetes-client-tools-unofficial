-- Copyright 2019 Fernando Rincon Martin
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- |
-- Module      :  Kubernetes.Tools.LeaderElection
-- Copyright   :  (c) 2019 Fernando Rincon Martin
-- License     :  Apache-2.0
-- Maintainer  :  Fernando Rincon <f.rincon@protonmail.com>
-- 
-- This module provides tools for do leader election

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Kubernetes.Tools.LeaderElection
  ( run'
  , runDefaultLeaseLock
  , LeaderElectionConfig(..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Data.Time.Clock
import Network.HTTP.Client (Manager)

import Kubernetes.OpenAPI.Client
import Kubernetes.OpenAPI.Core
import Kubernetes.OpenAPI.Logging
import Kubernetes.OpenAPI.Model

import Kubernetes.Tools.LeaderElection.LeaseLock
import Kubernetes.Util.Wait (jitterUntil)
import Kubernetes.Util.Time (diffTimeToMicroseconds)

data LeaderElectionConfig = LeaderElectionConfig
  { leaseDuration :: DiffTime
  , renewDeadline :: DiffTime
  , retryPeriod :: DiffTime
  , identity :: Text
  }

defaultJitterFactor :: Double
defaultJitterFactor = 1.2

-- | Runs a IO only when a lock can be acquired
run' :: IO lock -> (lock -> IO ()) -> IO a -> IO (Maybe a)
run' acquire renew work = do
  lock <- acquire
  result <- race (renew lock) work
  case result of 
    Left _ -> return Nothing
    Right r -> return $ Just r

acquireWithJitter :: DiffTime -> Double -> IO (Maybe a) -> IO a
acquireWithJitter retryPeriod jitterFactor acquire =
  jitterUntil retryPeriod jitterFactor acquire

-- | Runs a provided IO when a leader election lease is acquired
-- The IO provided will only starts when a lease can be acquired using the provided config.
-- Note: in case of loosing the lock, the IO will be cancelled throwing an asynchronows exception to the thread
runDefaultLeaseLock :: LeaderElectionConfig -> Manager -> KubernetesClientConfig -> Namespace -> Name -> IO a -> IO ()
runDefaultLeaseLock LeaderElectionConfig{..} manager kubeConfig ns name work = do
  forever $ run' acquire renew work
  where

    acquire = acquireWithJitter retryPeriod defaultJitterFactor $ runConfigLogWithExceptions "LeaseLock" kubeConfig $ do
      _log "LeaderElection" levelInfo "Trying to acquire lease lock for leader election"
      result <- liftIO $ acquireLock getCurrentTime manager kubeConfig leaseConfig
      case result of 
        Nothing -> _log "LeaderElection" levelInfo "Not getting the leader"
        Just r -> _log "LeaderElection" levelInfo [i|Now I am the leader!: #{r}|]
      return result

    leaseConfig = LeaseLockConfig 
      { leaseLockConfigName = name
      , leaseLockConfigNamespace = ns
      , leaseLockConfigHolderIndentity = HolderIdentity identity
      , leaseLockConfigLeaseDuration = leaseDuration
      }
    
    renew p = do
      result <- acquireLock getCurrentTime manager kubeConfig leaseConfig
      case result of 
        Nothing -> return ()
        Just _ -> do
          threadDelay $ diffTimeToMicroseconds renewDeadline
          renew p
