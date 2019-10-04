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
module Kubernetes.Tools.OperatorSpec where

import Test.Hspec

import Kubernetes.OpenAPI.Core
import Kubernetes.OpenAPI.Model
import Kubernetes.OpenAPI.ModelLens

import Kubernetes.Testing

import Kubernetes.Tools.Operator

import qualified Data.Text as T

import qualified Lens.Micro as L

import Kubernetes.GroupResourceVersion
import System.IO.Error (isUserError, ioeGetErrorString)

spec :: Spec
spec = describe "Operator" $ do
  describe "ensureCrd" $ do
    it "Should fail when there is no resource" $ do
      withMockedApi nullReaction $ \(manager, config) -> do
        ensureCrd manager config sampleCrd `shouldThrow` (\e -> isUserError e && (ioeGetErrorString e == "CRD Not Found"))
    it "Should not fail when the crd exists" $ do
      let expectedName = Name $ sampleName
      let  
        reactionChain action@GetAction{name = name} = 
          if 
            isGroupVersionResource apiextensionsV1beta1CustomResourceDefinition action &&
              name == expectedName
                then returnObject $ sampleCrd
                else notHandled
      withMockedApi reactionChain $ \(manager, config) -> do
        ensureCrd manager config sampleCrd
    it "Should fail when the crd is different" $ do
      let expectedName = Name $ sampleName
      let  
        reactionChain action@GetAction{name = name} = 
          if 
            isGroupVersionResource apiextensionsV1beta1CustomResourceDefinition action &&
              name == expectedName
                then returnObject $ sampleCrd
                else notHandled
      withMockedApi reactionChain $ \(manager, config) -> do
        ensureCrd manager config sampleCrd2 `shouldThrow` isUserError

sampleName :: T.Text
sampleName = T.intercalate "." [resource sampleGrv, group sampleGrv]

sampleGrv :: GroupResourceVersion
sampleGrv = GroupResourceVersion "example.com" "sampleresources" "v1alpha1"

sampleCrd :: V1beta1CustomResourceDefinition
sampleCrd = 
  sampleCrd'
    { v1beta1CustomResourceDefinitionMetadata = Just $ mkV1ObjectMeta
      { v1ObjectMetaName = Just sampleName }
    }
  where
    sampleCrd' = mkV1beta1CustomResourceDefinition sampleCrdSpec

    sampleCrdSpec = 
      mkV1beta1CustomResourceDefinitionSpec
        (group sampleGrv)
        sampleNames
        "namespaced"
    sampleNames =
      mkV1beta1CustomResourceDefinitionNames
        "SampleResource"
        "sampleresources"

sampleCrd2 :: V1beta1CustomResourceDefinition
sampleCrd2 = sampleCrd 
  { v1beta1CustomResourceDefinitionSpec = 
      mkV1beta1CustomResourceDefinitionSpec 
        (group sampleGrv)
        (mkV1beta1CustomResourceDefinitionNames "Other" "Others")
        "namespaced"
  }

    