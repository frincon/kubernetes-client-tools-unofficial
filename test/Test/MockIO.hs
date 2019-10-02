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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Test.MockIO
  ( buildMock1
  ) where

import Control.Exception (SomeException, try, throw)
import Data.IORef

buildMock1 ::  (p -> IO a) -> IO ((p -> IO a), IO [(p, Either SomeException a )])
buildMock1 f = do
  results <- newIORef []
  return (theFunction results, readIORef results)
  where 
    -- theFunction :: IORef [(p, Either SomeException a)] -> p -> IO a
    theFunction results par = do
      funcResult <- try $ f par
      modifyIORef results (\res -> (par, funcResult):res) 
      case funcResult of 
        Left e -> throw e
        Right a -> return a




