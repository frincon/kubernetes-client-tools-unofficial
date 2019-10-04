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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where
import Control.Concurrent.STM (atomically, newTVar)
import Data.Function          ((&))
import Kubernetes.Client      (KubeConfigSource (..), defaultTLSClientParams,
                                disableServerCertValidation,
                                disableServerNameValidation,
                                disableValidateAuthMethods, mkKubeClientConfig,
                                loadPEMCerts, newManager, setCAStore,
                                setClientCert, setMasterURI, setTokenAuth)
import Kubernetes.OpenAPI     (Accept (..), MimeJSON (..), dispatchMime,
                                newConfig)
import Network.TLS            (credentialLoadX509)
import qualified Data.Map                      as Map

import Kubernetes.Tools.Operator

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import Data.Text (pack)
import Data.Yaml (decodeThrow)
import Data.FileEmbed (embedFile)
import System.Directory (getHomeDirectory)

main :: IO ()
main = do
  homeFolder <- getHomeDirectory
  oidcCache <- atomically $ newTVar $ Map.fromList []
  (mgr, kcfg) <- mkKubeClientConfig oidcCache $ KubeConfigFile (homeFolder ++ "/.kube/config")
  crd <- decodeThrow $(embedFile "app/operator-example/operator-files/sample-crd.yaml")
  ensureCrd mgr kcfg crd
