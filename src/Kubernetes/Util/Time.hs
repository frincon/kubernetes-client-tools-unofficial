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
module Kubernetes.Util.Time 
  ( diffTimeToSeconds
  , diffTimeToMicroseconds
  ) where

import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import Data.Ratio 


diffTimeToSeconds :: (Num a) => DiffTime -> a
diffTimeToSeconds diffTime = fromInteger $ floor $ (diffTimeToPicoseconds diffTime) % picosecondsPerSecond

diffTimeToMicroseconds :: (Num a) => DiffTime -> a
diffTimeToMicroseconds diffTime = fromInteger $ floor $ (diffTimeToPicoseconds diffTime) % picosecondsPerMicrosecond

picosecondsPerSecond :: Integer
picosecondsPerSecond = 1000000000000

picosecondsPerMicrosecond :: Integer
picosecondsPerMicrosecond = 1000000