{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.CourseTask
  ( CourseTaskCreate(..)
  , CourseTaskDetails(..)
  , CourseTaskDetails'(..)
  , CourseTaskPatch(..)
  , courseTaskDetailFromModels
  , courseTaskDetailFromModels'
  , courseSolveFromModel
  ) where

import           Data.Aeson
import           Data.ByteString.Lazy          (fromStrict)
import           Data.Maybe                    (fromMaybe)
import           Data.Models.Course
import           Data.Models.CourseTaskPayload
import           Data.Models.StandCheck
import           Data.Models.User
import           Data.Text
import           Data.Text.Encoding
import           Database.Persist
import           Database.Persist.Sql
import           Foundation

data CourseTaskPatch = CourseTaskPatch
  { getCourseTaskPatchName    :: !(Maybe Text)
  , getCourseTaskPatchContent :: !(Maybe Text)
  , getCourseTaskPatchOrder   :: !(Maybe Int)
  , getCourseTaskPatchPayload :: !(Maybe CourseTaskPayload)
  }

instance FromJSON CourseTaskPatch where
  parseJSON = withObject "CourseTaskPatch" $ \v -> CourseTaskPatch
    <$> v .:? "name"
    <*> v .:? "content"
    <*> v .:? "order"
    <*> v .:? "payload"

data CourseTaskCreate = CourseTaskCreate
  { getCourseTaskCreateName    :: !Text
  , getCourseTaskCreateContent :: !Text
  , getCourseTaskCreateOrder   :: !Int
  , getCourseTaskCreatePayload :: !CourseTaskPayload
  }

instance FromJSON CourseTaskCreate where
  parseJSON = withObject "CourseTaskCreate" $ \v -> CourseTaskCreate
    <$> v .: "name"
    <*> v .: "content"
    <*> v .:? "order" .!= 0
    <*> v .: "payload"

data CourseTaskDetails = CourseTaskDetails
  { getCourseTaskDId       :: !Int
  , getCourseTaskDName     :: !Text
  , getCourseTaskDContent  :: !Text
  , getCourseTaskDOrder    :: !Int
  , getCourseTaskDCourse   :: !CourseDetails
  , getCourseTaskDPayload  :: !CourseTaskPayload
  , getCourseTaskDType     :: !CourseTaskType
  , getCourseTaskDAccepted :: !Bool
  }

data CourseTaskDetails' = CourseTaskDetails'
  { getCourseTaskDId'       :: !Int
  , getCourseTaskDName'     :: !Text
  , getCourseTaskDContent'  :: !Text
  , getCourseTaskDOrder'    :: !Int
  , getCourseTaskDCourse'   :: !CourseDetails
  , getCourseTaskDType'     :: !CourseTaskType
  , getCourseTaskDAccepted' :: !Bool
  }

data CourseTaskSolve = CourseTaskSolve
  { getCourseTaskSolveId      :: !Text
  , getCourseTaskSolveCorrect :: !Bool
  , getCourseTaskSolveInput   :: !Text
  }

instance ToJSON CourseTaskSolve where
  toJSON (CourseTaskSolve { .. }) = object
    [ "id" .= getCourseTaskSolveId
    , "correct" .= getCourseTaskSolveCorrect
    , "input" .= getCourseTaskSolveInput
    ]

courseSolveFromModel :: Entity CourseSolves -> CourseTaskSolve
courseSolveFromModel (Entity (CourseSolvesKey id') (CourseSolves { .. })) = CourseTaskSolve (pack id') courseSolvesCorrect (decodeUtf8 courseSolvesUserInput)

instance ToJSON CourseTaskDetails' where
  toJSON (CourseTaskDetails' { .. }) = object
    [ "id" .= getCourseTaskDId'
    , "name" .= getCourseTaskDName'
    , "content" .= getCourseTaskDContent'
    , "order" .= getCourseTaskDOrder'
    , "course" .= getCourseTaskDCourse'
    , "accepted" .= getCourseTaskDAccepted'
    , "type" .= getCourseTaskDType'
    ]

instance ToJSON CourseTaskDetails where
  toJSON (CourseTaskDetails { .. }) = object
    [ "id" .= getCourseTaskDId
    , "name" .= getCourseTaskDName
    , "content" .= getCourseTaskDContent
    , "order" .= getCourseTaskDOrder
    , "course" .= getCourseTaskDCourse
    , "payload" .= getCourseTaskDPayload
    , "accepted" .= getCourseTaskDAccepted
    , "type" .= getCourseTaskDType
    ]

courseTaskDetailFromModels' :: Entity CourseTask -> Entity Course -> Maybe UserDetails -> Bool -> CourseTaskDetails'
courseTaskDetailFromModels' (Entity ctId (CourseTask { .. })) e userDetails accepted = let
  taskId = fromSqlKey ctId
  courseDetails = courseDetailsFromModel e userDetails
  in CourseTaskDetails'
    { getCourseTaskDId' = fromIntegral taskId
    , getCourseTaskDName' = courseTaskName
    , getCourseTaskDContent' = courseTaskContent
    , getCourseTaskDOrder' = courseTaskOrderNumber
    , getCourseTaskDCourse' = courseDetails
    , getCourseTaskDType' = fromMaybe ContainerTask (courseTaskTypeFromString courseTaskType)
    , getCourseTaskDAccepted' = accepted
    }

courseTaskDetailFromModels :: Entity CourseTask -> Entity Course -> Maybe UserDetails -> Bool -> Either String CourseTaskDetails
courseTaskDetailFromModels (Entity ctId (CourseTask { .. })) e userDetails accepted = let
  taskId = fromSqlKey ctId
  courseDetails = courseDetailsFromModel e userDetails
  standPayload = case (eitherDecode . fromStrict) courseTaskPayload :: Either String CourseTaskPayload of
                  (Left e') -> Left $ "standActions: " <> e'
                  (Right r) -> Right r
  in do
    payload' <- standPayload
    return $ CourseTaskDetails
      { getCourseTaskDId = fromIntegral taskId
      , getCourseTaskDName = courseTaskName
      , getCourseTaskDContent = courseTaskContent
      , getCourseTaskDOrder = courseTaskOrderNumber
      , getCourseTaskDCourse = courseDetails
      , getCourseTaskDPayload = payload'
      , getCourseTaskDType = fromMaybe ContainerTask (courseTaskTypeFromString courseTaskType)
      , getCourseTaskDAccepted = accepted
      }
