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

module Main where

import           Data.Function                 ((&))
import           Kubernetes.Client             (defaultTLSClientParams,
                                                disableServerCertValidation,
                                                disableServerNameValidation,
                                                disableValidateAuthMethods,
                                                loadPEMCerts, newManager,
                                                setCAStore, setClientCert,
                                                setMasterURI)
import           Kubernetes.OpenAPI            (newConfig, configLogContext, stdoutLoggingContext)
import           Network.TLS                   (credentialLoadX509)

import Kubernetes.Tools.LeaderElection
import Kubernetes.OpenAPI.Model as M
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import Data.Text (pack)

main :: IO ()
main = do
    -- We need to first create a Kubernetes.Core.KubernetesConfig and a Network.HTTP.Client.Manager.
    -- Currently we need to construct these objects manually. Work is underway to construct these
    -- objects automatically from a kubeconfig file. See https://github.com/kubernetes-client/haskell/issues/2.
    kcfg0 <-
        newConfig
        & fmap (setMasterURI "https://192.168.99.100:8443")    -- fill in master URI
        -- & fmap (setTokenAuth "mytoken")                          -- if using token auth
        & fmap disableValidateAuthMethods                        -- if using client cert auth
    logContext <- stdoutLoggingContext $ configLogContext kcfg0
    let kcfg = kcfg0 { configLogContext = logContext}
    myCAStore <- loadPEMCerts "/home/fernando/.minikube/ca.crt"                  -- if using custom CA certs
    myCert    <-                                                 -- if using client cert
        credentialLoadX509 "/home/fernando/.minikube/client.crt" "/home/fernando/.minikube/client.key"
            >>= either error return
    tlsParams <-
        defaultTLSClientParams
        & fmap disableServerNameValidation -- if master address is specified as an IP address
        & fmap disableServerCertValidation -- if you don't want to validate the server cert at all (insecure)
        & fmap (setCAStore myCAStore)      -- if using custom CA certs
        & fmap (setClientCert myCert)      -- if using client cert
    manager <- newManager tlsParams
    args <- getArgs
    if length args /= 1
      then putStrLn "A argument with the identity is required"
      else runDefaultLeaseLock (leaderElectionConfig (pack $ head args)) manager kcfg (Namespace "default") (Name "testing") $ do
        forever $ do
          putStrLn "I am the leader nowwwwww"
          threadDelay 1000000
    where 
      leaderElectionConfig identity = LeaderElectionConfig 
        { leaseDuration = 30
        , renewDeadline = 20
        , retryPeriod = 10
        , identity = identity
        }
    
