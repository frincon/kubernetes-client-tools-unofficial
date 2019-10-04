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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Kubernetes.Tools.Operator where

import Development.Placeholders

import Kubernetes.GroupResourceVersion
import Kubernetes.OpenAPI.Client
import Kubernetes.OpenAPI.Core
import Kubernetes.OpenAPI.MimeTypes
import Kubernetes.OpenAPI.Logging
import Kubernetes.OpenAPI.Model as M
import Kubernetes.OpenAPI.API.ApiextensionsV1beta1

import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Types.Status as NH
import Data.String.Interpolate (i)

import qualified Data.Text as T

import System.IO.Error (userError, ioError)

ensureCrd :: Manager -> KubernetesClientConfig -> V1beta1CustomResourceDefinition -> IO ()
ensureCrd manager config V1beta1CustomResourceDefinition{..} = 
  case v1beta1CustomResourceDefinitionMetadata of
    Nothing -> fail "CRD Provided does not have metadata"
    Just metadata -> case v1ObjectMetaName metadata of
      Nothing -> fail "CRD Provided does not have a name"
      Just name -> do
        result <- dispatchMime' manager config $ readCustomResourceDefinition (Accept MimeJSON) (Name name)
        case result of 
          Right crd -> 
            if compareSelectivelly (M.v1beta1CustomResourceDefinitionSpec crd) v1beta1CustomResourceDefinitionSpec
              then return ()
              else ioError $ userError [i|CRD Found but does not match with the crd provided. Spec: #{M.v1beta1CustomResourceDefinitionSpec crd}|]
          Left err -> 
            if (NH.responseStatus $ mimeErrorResponse err) == NH.notFound404
              then ioError (userError "CRD Not Found")
              else fail [i|Unexpected result while trying to read crd: #{err}|]
  where
    compareSelectivelly spec1 spec2 = 
      (v1beta1CustomResourceDefinitionSpecVersions spec1 == v1beta1CustomResourceDefinitionSpecVersions spec2)
      &&
      (v1beta1CustomResourceDefinitionSpecValidation spec1 == v1beta1CustomResourceDefinitionSpecValidation spec2)
      &&
      compareNames (v1beta1CustomResourceDefinitionSpecNames spec1) (v1beta1CustomResourceDefinitionSpecNames spec2)
    compareNames names1 names2 = 
      (v1beta1CustomResourceDefinitionNamesKind names1 == v1beta1CustomResourceDefinitionNamesKind names2)
      &&
      (v1beta1CustomResourceDefinitionNamesPlural names1 == v1beta1CustomResourceDefinitionNamesPlural names2)
      -- TODO Maybe we need to check for not nothing in the second argument

