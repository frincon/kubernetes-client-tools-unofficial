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
module Kubernetes.Tools.LeaderElectionSpec where

import Test.Hspec
import Test.MockIO

import Kubernetes.Tools.LeaderElection

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled)
import System.IO.Error (isUserError)
import Control.Exception (fromException)
import Control.Monad (forever)
import Test.QuickCheck (property)
import Data.Maybe (isJust)
import Data.Either (isLeft)

spec :: Spec
spec = do
  describe "run'" $ do
    it "When acquire fails, work is not called" $ do
      run' (fail "Testing fail") undefined undefined `shouldThrow` isUserError
    it "When acquire works, then the work is called" $ property $ \a -> 
      run' (return ()) (\_ -> forever $ threadDelay 1000000) (return a) `shouldReturn` (Just a :: Maybe Int)
    it "When the work ends, the renew is cancelled" $ do
      (mocked, mock) <- buildMock1 runForEver
      _ <- run' (return ()) mocked (threadDelay 100 >> return ()) 
      result <- mock
      length result `shouldBe` 1
      snd (head result) `shouldSatisfy` isLeft
      let Left exception = snd (head result)
      (fromException exception :: Maybe AsyncCancelled) `shouldSatisfy` isJust
      
runForEver :: a -> IO b
runForEver _ = forever $ threadDelay 10000000
