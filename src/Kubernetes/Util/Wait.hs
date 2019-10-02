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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.Util.Wait
  ( jitterUntil
  ) where

import Data.Time.Clock
import Control.Concurrent (threadDelay)
import Kubernetes.Util.Time (diffTimeToMicroseconds)
import System.Random (randomIO)

-- TODO Maybe we can use directly the retry package:
-- http://hackage.haskell.org/package/retry

jitterUntil :: DiffTime -> Double -> IO (Maybe a) -> IO a
jitterUntil duration maxFactor action = do
  jitterDuration <- jitter duration maxFactor
  result <- action
  case result of 
    Nothing -> do
      threadDelay $ diffTimeToMicroseconds jitterDuration
      jitterUntil duration maxFactor action
    Just a -> return a

jitter :: DiffTime -> Double -> IO DiffTime
jitter duration maxFactor = do
  random <- randomIO
  return $ duration + realToFrac (random * (realToFrac duration) * maxFactor')
    where 
      maxFactor' = if maxFactor <= 0.0
        then 1.0
        else maxFactor
