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
-- Module      :  Kubernetes.Tools.LeaderElection.LeaseLock
-- Copyright   :  (c) 2019 Fernando Rincon Martin
-- License     :  Apache-2.0
-- Maintainer  :  Fernando Rincon <f.rincon@protonmail.com>
-- 
-- This module provides tools for do leader election based on Leases resources
--
-- Note that it requires a cluster that support V1Coordination API (v1.13.0)

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.Tools.LeaderElection.LeaseLock
  ( acquireLock
  , LeaseLockConfig(..)
  , HolderIdentity(..)
  ) where

import Kubernetes.OpenAPI.Core
import Kubernetes.OpenAPI.Client
import Kubernetes.OpenAPI.MimeTypes
import Kubernetes.OpenAPI.Model
import Kubernetes.OpenAPI.API.CoordinationV1

import Data.String.Interpolate (i)

import Kubernetes.Util.Time

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Text (Text)
import Data.Time.Clock (UTCTime, DiffTime, addUTCTime)

import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Types.Status as NH

newtype HolderIdentity = HolderIdentity { unHolderIdentity :: Text } deriving (Eq, Show)

data LeaseLockConfig = LeaseLockConfig
  { leaseLockConfigName :: Name
  , leaseLockConfigNamespace :: Namespace
  , leaseLockConfigHolderIndentity :: HolderIdentity
  , leaseLockConfigLeaseDuration :: DiffTime
  } deriving (Show)

acquireLock :: MonadIO m => IO UTCTime -> NH.Manager -> KubernetesClientConfig -> LeaseLockConfig -> m (Maybe V1Lease)
acquireLock getCurrentTime manager config leaseLockConfig@LeaseLockConfig{..} = do
  now <- liftIO $ getCurrentTime
  res <- liftIO $ dispatchMime' manager config $ readNamespacedLease (Accept MimeJSON) leaseLockConfigName leaseLockConfigNamespace
  case res of
    Left err -> checkForError err
    Right lease -> checkForExistingLease now lease
  where

    checkForError :: MonadIO m => MimeError -> m (Maybe V1Lease)
    checkForError MimeError{..} = 
      if NH.responseStatus mimeErrorResponse == NH.notFound404
        then createNewLease
        else fail [i|Error getting the lease, return status '#{NH.responseStatus mimeErrorResponse}' and message: #{mimeError}|]
    
    createNewLease :: MonadIO m => m (Maybe V1Lease)
    createNewLease = do
      currentTime <- liftIO $ getCurrentTime
      result <- liftIO $ dispatchMime' manager config $ createNamespacedLease (ContentType MimeJSON) (Accept MimeJSON) (newLease currentTime) leaseLockConfigNamespace
      case result of 
        Right lease -> return $ Just lease
        Left err -> if (NH.responseStatus $ mimeErrorResponse err) == NH.conflict409
          then return Nothing
          else fail [i|Error creating a new lease: #{mimeError err}, response:\n#{NH.responseBody $ mimeErrorResponse $ err}|]

    newLease :: UTCTime -> V1Lease
    newLease acquireTime = 
      mkV1Lease
        { v1LeaseMetadata = Just $ mkV1ObjectMeta 
          { v1ObjectMetaName = Just $ unName leaseLockConfigName
          , v1ObjectMetaNamespace = Just $ unNamespace leaseLockConfigNamespace
          }
        , v1LeaseSpec = Just $ mkV1LeaseSpec
          { v1LeaseSpecAcquireTime = Just $ DateTime acquireTime
          , v1LeaseSpecHolderIdentity = Just $ unHolderIdentity leaseLockConfigHolderIndentity
          , v1LeaseSpecLeaseDurationSeconds = Just $ diffTimeToSeconds leaseLockConfigLeaseDuration
          , v1LeaseSpecRenewTime = Just $ DateTime acquireTime
          , v1LeaseSpecLeaseTransitions = Just 1
          }
        }

    -- TODO Check what is the error in case Nothing comes in the spec
    -- but this is not likely to heppen as it is required by the API
    checkForExistingLease :: MonadIO m => UTCTime -> V1Lease -> m (Maybe V1Lease)
    checkForExistingLease now lease@V1Lease{v1LeaseSpec = Just leaseSpec} = 
      if isValid now leaseSpec && isDifferentIdentity leaseSpec
        then return Nothing
        else do
          result <- renewLease now lease
          case result of 
            Nothing -> liftIO $ acquireLock getCurrentTime manager config leaseLockConfig
            Just _ -> return result
    checkForExistingLease _ V1Lease{v1LeaseSpec = Nothing } = fail "Lease does not have a spec"


    renewLease :: MonadIO m => UTCTime -> V1Lease -> m (Maybe V1Lease)
    renewLease now lease@V1Lease
      { v1LeaseSpec = Just leaseSpec@V1LeaseSpec
        { v1LeaseSpecLeaseTransitions = Just transitions, v1LeaseSpecHolderIdentity = Just holderIdentity
        }
      } = do
      result <- liftIO $ dispatchMime' manager config $ replaceRequest
      case result of 
        Left err -> if (NH.responseStatus $ mimeErrorResponse err) == NH.conflict409
          then return Nothing
          else fail [i|Failure when trying to renew the lease: #{err}|]
        Right l -> return $ Just l
      where
        replaceRequest = replaceNamespacedLease (ContentType MimeJSON) (Accept MimeJSON) changedLease leaseLockConfigName leaseLockConfigNamespace
        changedLease = lease
          { v1LeaseSpec = Just leaseSpec 
            { v1LeaseSpecRenewTime = Just $ DateTime now 
            , v1LeaseSpecHolderIdentity = Just $ unHolderIdentity leaseLockConfigHolderIndentity
            , v1LeaseSpecLeaseDurationSeconds = Just $ diffTimeToSeconds leaseLockConfigLeaseDuration
            , v1LeaseSpecLeaseTransitions = Just $ transitions + (if holderIdentity == unHolderIdentity leaseLockConfigHolderIndentity then 0 else 1)
            }
          }
    renewLease _ _ = fail "Received a lease with missing fields"

    isValid :: UTCTime -> V1LeaseSpec -> Bool
    isValid now V1LeaseSpec
      { v1LeaseSpecRenewTime=Just (DateTime renewTime)
      , v1LeaseSpecLeaseDurationSeconds= Just durationSeconds
      } = addUTCTime (fromIntegral durationSeconds) renewTime >= now
    isValid _ _ = error "Found a lease without renew time and/or duration"

    isDifferentIdentity :: V1LeaseSpec -> Bool
    isDifferentIdentity V1LeaseSpec
      { v1LeaseSpecHolderIdentity = Just otherIdentity
      } = otherIdentity /= unHolderIdentity leaseLockConfigHolderIndentity
    isDifferentIdentity _ = error "Found a lease without identity"


