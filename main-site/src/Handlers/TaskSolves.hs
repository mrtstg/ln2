{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.TaskSolves
  ( postApiTaskSolvesR
  , getApiTaskSolvesR
  ) where

import           Api.Login              (requireApiAuth)
import           Api.Task
import           Crud.Course
import           Crud.TaskSolves
import           Data.Aeson
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Models.CourseTask
import           Data.Models.StandCheck
import           Data.Models.User
import           Data.Text
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Time.Clock
import           Foundation
import           Handlers.Utils
import           Network.HTTP.Types
import           Yesod.Core
import           Yesod.Persist

newtype TaskAnswer = TaskAnswer Text deriving (Show, Eq)

instance FromJSON TaskAnswer where
  parseJSON = withObject "TaskAnswer" $ \v -> TaskAnswer <$> v .: "answer"

getApiTaskSolvesR :: CourseTaskId -> Handler Value
getApiTaskSolvesR ctId = do
  d@(UserDetails { .. }) <- requireApiAuth
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
            (solves, solvesA) <- getTaskSolves pageN d ctId
            sendStatusJSON status200 $ object
              [ "total" .= solvesA
              , "pageSize" .= defaultPageSize
              , "objects" .= Prelude.map courseSolveFromModel solves
              ]

postApiTaskSolvesR :: CourseTaskId -> Handler Value
postApiTaskSolvesR ctId = do
  d <- requireApiAuth
  (TaskAnswer ans) <- requireCheckJsonBody
  courseTaskRes <- runDB $ selectFirst [ CourseTaskId ==. ctId ] []
  case courseTaskRes of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Task not found!" ]
    (Just ct) -> do
      (status, response) <- createTaskSolve ans d ct
      case status of
        n | n `Prelude.elem` [400, 403] -> sendStatusJSON (if status == 403 then status403 else status400) $ object [ "error" .= (String . pack) response ]
        200 -> sendStatusJSON status200 $ object ["uuid" .= response]
        _unexpectedStatus -> sendStatusJSON status500 $ object ["error" .= String "Sometyhing went wrong!"]
