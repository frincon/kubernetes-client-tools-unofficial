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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Kubernetes.Tools.LeaderElection.LeaseLockSpec where

import Test.Hspec

import Development.Placeholders

import Kubernetes.Tools.LeaderElection.LeaseLock
import Kubernetes.OpenAPI.Core
import Kubernetes.OpenAPI.Client
import Kubernetes.OpenAPI.MimeTypes
import Kubernetes.OpenAPI.Model
import Kubernetes.OpenAPI.API.CoordinationV1

import Kubernetes.Testing

import qualified Kubernetes.GroupResourceVersion as GRV

import Data.Maybe (isJust)
import Network.HTTP.Types.Version
import Data.Time.Clock (UTCTime(..), addUTCTime)
import Data.Time.Calendar (fromGregorian)

spec :: Spec
spec = describe "LeaseLock" $ do
  describe "acquireLock" $ do
    it "Should return nothing when there is another lease" $ do
      let name = Name "aName"
      let namespace = Namespace "aNamespace"
      let identity1 = HolderIdentity "identity1"
      let identity2 = HolderIdentity "identity2"
      let now = sampleTime
      
      let 
        reactionChain action@GetAction{..} = if isGroupVersionResource GRV.coordinationV1Lease action 
          then returnObject $ mkLeaseValid name namespace identity1 now
          else notHandled
        reactionChain _ = return Nothing

      withMockedApi reactionChain $ \(manager, config) -> do
          acquireLock (return now) manager config (LeaseLockConfig name namespace identity2 10) `shouldReturn` Nothing
    it "Should return new lease when there is no another lease" $ do
      let name = Name "aName"
      let namespace = Namespace "aNamespace"
      let identity1 = HolderIdentity "identity1"
      let now = sampleTime

      let 
        reactionChain = 
          buildReactionChain 
            [ onGetAction (isGroupVersionResource GRV.coordinationV1Lease) notFound
            , onCreateAction (isGroupVersionResource GRV.coordinationV1Lease) creationOk
            ]

      withMockedApi reactionChain $ \(manager, config) -> do
          result <- acquireLock (return now) manager config (LeaseLockConfig name namespace identity1 10)
          result `shouldSatisfy` isJust

    it "Should return new lease when there is another expired lease" $ do
      let name = Name "aName"
      let namespace = Namespace "aNamespace"
      let identity1 = HolderIdentity "identity1"
      let identity2 = HolderIdentity "identity2"
      let now = sampleTime
      
      returnOnlyOnce <- onlyOnce $ onGetAction (isGroupVersionResource GRV.coordinationV1Lease) (returnObject $ mkLeaseValid name namespace identity1 (addUTCTime (-120) now))

      let
        reactionChain = 
          buildReactionChain 
            [ returnOnlyOnce
            , onDeleteAction (isGroupVersionResource GRV.coordinationV1Lease) deletionOk 
            , onCreateAction (isGroupVersionResource GRV.coordinationV1Lease) creationOk
            ]

      withMockedApi reactionChain $ \(manager, config) -> do
          result <- acquireLock (return now) manager config (LeaseLockConfig name namespace identity2 10)
          result `shouldSatisfy` isJust

mkLeaseValid name namespace holderIdentity validityInstant = 
  mkV1Lease 
    { v1LeaseMetadata = Just $ mkV1ObjectMeta 
      { v1ObjectMetaName = Just $ unName name
      , v1ObjectMetaNamespace = Just $ unNamespace namespace
      }
    , v1LeaseSpec = Just $ mkV1LeaseSpec
      { v1LeaseSpecAcquireTime = Just $ DateTime $ addUTCTime (-60) validityInstant 
      , v1LeaseSpecHolderIdentity = Just $ unHolderIdentity holderIdentity
      , v1LeaseSpecLeaseDurationSeconds = Just $ 20
      , v1LeaseSpecRenewTime = Just $ DateTime $ addUTCTime (-10) validityInstant
      , v1LeaseSpecLeaseTransitions = Just 1
      }
    }


sampleTime = UTCTime (fromGregorian 2019 9 15) 60