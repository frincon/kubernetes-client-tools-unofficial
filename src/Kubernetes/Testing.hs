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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Kubernetes.Testing
  ( withMockedApi
  , isGroupVersionResource
  , returnObject
  , notHandled
  , notFound
  , buildReactionChain
  , onGetAction
  , onCreateAction
  , creationOk
  , onDeleteAction
  , deletionOk
  , onReplaceAction
  , replaceOk
  , onlyOnce
  , Action(..)
  ) where

import Kubernetes.OpenAPI.Core
import Kubernetes.OpenAPI.Model
import Kubernetes.Client
import Kubernetes.GroupResourceVersion
import Data.IORef

import           Data.Function                 ((&))

import Network.HTTP.Mock

import Network.Wai
import Data.String.Interpolate (i)
import Data.Aeson

import qualified Network.HTTP.Client as NH
import qualified Data.ByteString.Lazy.Char8 as BCL

import Network.HTTP.Types.Status
import Network.HTTP.Types.Method

data Action 
  = GetAction 
    { groupResourceVersion :: GroupResourceVersion
    , name :: Name
    }
  | CreateAction
    { groupResourceVersion :: GroupResourceVersion
    , object :: Value
    }
  | DeleteAction
    { groupResourceVersion :: GroupResourceVersion
    , name :: Name
    , deleteOptions :: Maybe V1DeleteOptions
    }
  | ReplaceAction
    { groupResourceVersion :: GroupResourceVersion
    , object :: Value
    }
  deriving (Show)

data ReactionResult
  = ReturnObject { object :: Value }
  | ReturnStatus { status :: Status }
  deriving (Show)

type ReactionFunc = Action -> IO (Maybe ReactionResult)

onGetAction :: (Action -> Bool) -> IO (Maybe ReactionResult) -> ReactionFunc
onGetAction cond result action@GetAction{} = if cond action then result else notHandled
onGetAction _ _ _ = notHandled

onCreateAction :: (Action -> Bool) -> (Action -> IO (Maybe ReactionResult)) -> ReactionFunc
onCreateAction cond resultFunc action@CreateAction{} = if cond action then resultFunc action else notHandled
onCreateAction _ _ _ = notHandled

onDeleteAction :: (Action -> Bool) -> (Action -> IO (Maybe ReactionResult)) -> ReactionFunc
onDeleteAction cond resultFunc action@DeleteAction{} = if cond action then resultFunc action else notHandled
onDeleteAction _ _ _ = notHandled

onReplaceAction :: (Action -> Bool) -> (Action -> IO (Maybe ReactionResult)) -> ReactionFunc
onReplaceAction cond resultFunc action@ReplaceAction{} = if cond action then resultFunc action else notHandled
onReplaceAction _ _ _ = notHandled

onlyOnce :: ReactionFunc -> IO ReactionFunc
onlyOnce f = do
  alreadyUsed <- newIORef False
  return $ \action -> do
    alreadyUsed' <- atomicModifyIORef alreadyUsed (\v -> (True, v))
    if not alreadyUsed'
      then f action
      else notHandled

isGroupVersionResource :: GroupResourceVersion -> Action -> Bool
isGroupVersionResource expectedGrv GetAction{ groupResourceVersion = actualGrv } = actualGrv == expectedGrv
isGroupVersionResource expectedGrv CreateAction{ groupResourceVersion = actualGrv } = actualGrv == expectedGrv
isGroupVersionResource expectedGrv DeleteAction{ groupResourceVersion = actualGrv } = actualGrv == expectedGrv
isGroupVersionResource expectedGrv ReplaceAction{ groupResourceVersion = actualGrv } = actualGrv == expectedGrv

buildReactionChain :: [ReactionFunc] -> ReactionFunc
buildReactionChain (f:fs) action = do
  result <- f action
  case result of
    Nothing -> buildReactionChain fs action
    Just _ -> return result
buildReactionChain [] _ = return Nothing

returnObject :: ToJSON a => a -> IO (Maybe ReactionResult)
returnObject theObject = return $ Just $ ReturnObject $ toJSON theObject

notHandled :: IO (Maybe ReactionResult)
notHandled = return Nothing

notFound :: IO (Maybe ReactionResult)
notFound = return $ Just $ ReturnStatus status404

creationOk :: Action -> IO (Maybe ReactionResult)
-- TODO Maybe we need to review the object parameters metadata, as they can come missing
creationOk (CreateAction _ obj) = return $ Just $ ReturnObject obj 
creationOk _ = fail "Creation ok expects a create action"

deletionOk :: Action -> IO (Maybe ReactionResult)
-- TODO Maybe we need to review the object parameters metadata, as they can come missing
deletionOk (DeleteAction _ _ _) = return $ Just $ ReturnObject $ toJSON $ mkV1Status 
  { v1StatusStatus = Just "Success"
  , v1StatusKind = Just "Status"
  , v1StatusApiVersion = Just "v1"
  -- , v1StatusDetails = $notImplemented -- TODO Make the details
  }
deletionOk _ = fail "Deletion ok expects a delete action"

replaceOk :: Action -> IO (Maybe ReactionResult)
-- TODO Maybe we need to change the resourceVersion
replaceOk (ReplaceAction _ obj) = return $ Just $ ReturnObject obj
replaceOk _ = fail "Replace ok expects a replace action"

withMockedApi :: ReactionFunc -> ((NH.Manager, KubernetesClientConfig) -> IO a) -> IO a
withMockedApi reactionFunc f = do
    kcfg <- mockedConfig
    withMockedManager (waiApp reactionFunc) $ \manager -> f (manager, kcfg)

waiApp :: ReactionFunc -> Application
waiApp reactionFunc req respond = do
  body <- strictRequestBody req
  let actionOrError = toAction req body
  case actionOrError of
    Left errorMsg -> fail errorMsg
    Right action -> do
      reactionResult <- reactionFunc action
      case reactionResult of
        Nothing -> respond $ responseLBS status404 [] "Reaction function not handle the request"
        Just result -> respond $ toResponse result

toAction :: Request -> BCL.ByteString -> Either String Action
toAction req body = case requestMethod req of
  method 
    | method == methodGet -> do 
      grv <- extractGrv req
      resourceName <- extractName req
      return $ GetAction grv resourceName
    | method == methodPost -> do
      grv <- extractGrv req
      objectBody <- eitherDecode body
      return $ CreateAction grv objectBody
    | method == methodDelete -> do
      grv <- extractGrv req
      resourceName <- extractName req
      maybeOptions <- extractDeleteOptions body
      return $ DeleteAction grv resourceName maybeOptions
    | method == methodPut -> do
      grv <- extractGrv req
      objectBody <- eitherDecode body
      return $ ReplaceAction grv objectBody
    | otherwise -> fail [i|Method not expected #{method}|]

extractDeleteOptions :: BCL.ByteString -> Either String (Maybe V1DeleteOptions)
extractDeleteOptions body = 
  if BCL.null body
    then Right Nothing
    else case eitherDecode body of
      Right options -> Right $ Just options
      Left msg -> Left msg

extractGrv :: Request -> Either String GroupResourceVersion
extractGrv req = 
  let
    validate msg cond = if cond then return () else fail msg
    paths = pathInfo req
  in do
    validate "The path should have at least 4 parts" (length paths > 3)
    validate "The path should start '/apis/'" (paths !! 0 == "apis")
    -- TODO Core resources does not have group
    if length paths >= 6 && (paths !! 3) == "namespaces" 
      then return $ GroupResourceVersion { group = paths !! 1, version = paths !! 2, resource = paths !! 5}
      else return $ GroupResourceVersion { group = paths !! 1, version = paths !! 2, resource = paths !! 3}

extractName :: Request -> Either String Name
extractName req = 
  let
    validate msg cond = if cond then return () else fail msg
    paths = pathInfo req
  in do
    validate "The path should have at least 4 parts" (length paths > 3)
    validate "The path should start '/apis/'" (paths !! 0 == "apis")
    -- TODO Core resources does not have group
    if length paths >= 6 && (paths !! 3) == "namespaces" 
      then return $ Name (paths !! 6)
      else return $ Name (paths !! 4)
      

toResponse :: ReactionResult -> Response
toResponse (ReturnObject theValue) = responseLBS status200 [] (encode theValue)
toResponse (ReturnStatus theStatus) = responseLBS theStatus [] ""

mockedConfig :: IO KubernetesClientConfig
mockedConfig = 
  newConfig
  & fmap disableValidateAuthMethods
