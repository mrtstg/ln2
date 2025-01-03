{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Deployments
  ( getDeploymentsR
  , getDeploymentsApiR
  ) where

import           Api
import           Api.Deploy.User
import           Data.Functor                ((<&>))
import           Data.Models.Deployment
import           Data.Models.User
import qualified Data.Text                   as T
import           Database.Persist
import           Database.Persist.Postgresql (toSqlKey)
import           Foundation
import           Handlers.Auth
import           Handlers.Params
import           Handlers.Utils
import           Network.HTTP.Types
import           Yesod.Core
import           Yesod.Persist

getDeploymentsR :: Handler Html
getDeploymentsR = do
  _ <- requireUserAuth
  defaultLayout $ do
    setTitle "Развертывания"
    [whamlet|
<div .container.pt-2.py-3>
  <h1 .title.is-3> Активные развертывания
  <div #app>
<script src=/static/js/deployments.js>
|]

linkDeployments :: [Deployment] -> Handler [Deployment]
linkDeployments = helper [] where
  helper :: [Deployment] -> [Deployment] -> Handler [Deployment]
  helper acc [] = (pure . reverse) acc
  helper acc (d@(Deployment { .. }):lst) = do
    let taskId = (toSqlKey . fromIntegral) getDeploymentTaskId
    taskExists <- runDB $ exists [ CourseTaskId ==. taskId ]
    if not taskExists then helper (d:acc) lst else do
      (task', course') <- runDB $ do
        t <- selectFirst [ CourseTaskId ==. taskId ] []
        c <- selectFirst [ CourseId ==. (CourseKey . T.unpack) getDeploymentCourseId ] []
        pure (t, c)
      case (task', course') of
        (Just (Entity _ CourseTask { .. }), Just (Entity _ Course { .. })) -> do
          helper (d { getDeploymentCourseName = Just courseName, getDeploymentTaskName = Just courseTaskName }:acc) lst
        (_, _) -> helper (d:acc) lst

getDeploymentsApiR :: Handler Value
getDeploymentsApiR = do
  App { endpointsConfiguration = endpoints } <- getYesod
  pageN <- getPageNumber
  (UserDetails { .. }) <- requireApiAuthF userAuthFilter <&> userAuthMap
  deployments' <- liftIO $ getUserDeployments' endpoints pageN getUserDetailsId False
  case deployments' of
    (Left e) -> sendStatusJSON status500 $ object [ "error" .= e]
    (Right (ApiPageWrapper { .. })) -> do
      linkedDeployments <- linkDeployments getPageWrapperObjects
      sendStatusJSON status200 $ ApiPageWrapper
        { getPageWrapperTotal = getPageWrapperTotal
        , getPageWrapperSize = getPageWrapperSize
        , getPageWrapperObjects = linkedDeployments
        }
