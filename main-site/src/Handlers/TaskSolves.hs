{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.TaskSolves
  ( postApiTaskSolvesR
  , getApiTaskSolvesR
  ) where

import           Crud.TaskSolves
import           Data.Aeson
import           Data.Models.CourseTask
import           Data.Models.User
import           Data.Text
import           Data.Time.Clock
import           Foundation
import           Handlers.Auth
import           Handlers.Params
import           Network.HTTP.Types
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

newtype TaskAnswer = TaskAnswer Text deriving (Show, Eq)

instance FromJSON TaskAnswer where
  parseJSON = withObject "TaskAnswer" $ \v -> TaskAnswer <$> v .: "answer"

getApiTaskSolvesR :: CourseTaskId -> Handler Value
getApiTaskSolvesR ctId = do
  App { endpointsConfiguration = endpoints } <- getYesod
  UserDetails { .. } <- requireApiUserAuth endpoints
  pageN <- getPageNumber
  courseTaskRes <- runDB $ selectFirst [ CourseTaskId ==. ctId ] []
  case courseTaskRes of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Task not found!" ]
    (Just (Entity _ (CourseTask { .. }))) -> do
      courseRes <- runDB $ selectFirst [ CourseId ==. courseTaskCourse ] []
      case courseRes of
        Nothing -> error "Unreachable pattern!"
        (Just (Entity (CourseKey courseUUID) _)) -> do
          let isMember = isUserCourseMember courseUUID getUserRoles
          if not isMember then sendStatusJSON status403 $ object [ "error" .= String "You have no access to course!" ] else do
            -- TODO: получение
            (solves, solvesA) <- getTaskSolves pageN getUserDetailsId ctId
            sendStatusJSON status200 $ object
              [ "total" .= solvesA
              , "pageSize" .= defaultPageSize
              , "objects" .= Prelude.map courseSolveFromModel solves
              ]

postApiTaskSolvesR :: CourseTaskId -> Handler Value
postApiTaskSolvesR ctId = do
  reqTime <- liftIO getCurrentTime
  App { endpointsConfiguration = endpoints } <- getYesod
  d <- requireApiUserAuth endpoints
  (TaskAnswer ans) <- requireCheckJsonBody
  courseTaskRes <- runDB $ selectFirst [ CourseTaskId ==. ctId ] []
  case courseTaskRes of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Task not found!" ]
    (Just ct) -> do
      reachedTimeout <- isUserReachedSolveTimeout d reqTime
      if reachedTimeout then sendStatusJSON status429 $ object [ "error" .= String "Reached timeout!" ] else do
        (status, response) <- createTaskSolve ans d ct
        case status of
          n | n `Prelude.elem` [400, 403] -> sendStatusJSON (if status == 403 then status403 else status400) $ object [ "error" .= (String . pack) response ]
          200 -> sendStatusJSON status200 $ object ["uuid" .= response]
          _unexpectedStatus -> sendStatusJSON status500 $ object ["error" .= String "Sometyhing went wrong!"]
