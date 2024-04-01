{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.CourseTask
  ( CourseTaskCreate(..)
  , CourseTaskDetails(..)
  , CourseTaskDetails'(..)
  , courseTaskDetailFromModels
  , courseTaskDetailFromModels'
  , courseSolveFromModel
  ) where

import           Data.Aeson
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Models.Course
import           Data.Models.StandCheck
import           Data.Models.User
import           Data.Text
import           Data.Text.Encoding
import           Database.Persist
import           Database.Persist.Sql
import           Foundation

data CourseTaskCreate = CourseTaskCreate
  { getCourseTaskCreateName            :: !Text
  , getCourseTaskCreateContent         :: !Text
  , getCourseTaskCreateOrder           :: !Int
  , getCourseTaskCreateStandIdentifier :: !Text
  , getCourseTaskCreateStandActions    :: ![StandCheckStage]
  }

instance FromJSON CourseTaskCreate where
  parseJSON = withObject "CourseTaskCreate" $ \v -> CourseTaskCreate
    <$> v .: "name"
    <*> v .: "content"
    <*> v .:? "order" .!= 0
    <*> v .: "standIdentifier"
    <*> v .: "standActions"

data CourseTaskDetails = CourseTaskDetails
  { getCourseTaskDId           :: !Int
  , getCourseTaskDName         :: !Text
  , getCourseTaskDContent      :: !Text
  , getCourseTaskDOrder        :: !Int
  , getCourseTaskDCourse       :: !CourseDetails
  , getCourseTaskDStand        :: !Text
  , getCourseTaskDStandActions :: ![StandCheckStage]
  }

data CourseTaskDetails' = CourseTaskDetails'
  { getCourseTaskDId'       :: !Int
  , getCourseTaskDName'     :: !Text
  , getCourseTaskDContent'  :: !Text
  , getCourseTaskDOrder'    :: !Int
  , getCourseTaskDCourse'   :: !CourseDetails
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
    ]

instance ToJSON CourseTaskDetails where
  toJSON (CourseTaskDetails { .. }) = object
    [ "id" .= getCourseTaskDId
    , "name" .= getCourseTaskDName
    , "content" .= getCourseTaskDContent
    , "order" .= getCourseTaskDOrder
    , "course" .= getCourseTaskDCourse
    , "standIdentifier" .= getCourseTaskDStand
    , "standActions" .= getCourseTaskDStandActions
    ]

courseTaskDetailFromModels' :: Entity CourseTask -> Entity Course -> Maybe UserDetails -> Bool -> CourseTaskDetails'
courseTaskDetailFromModels' (Entity ctId (CourseTask { .. })) e userDetails accepted = let
  taskId = fromSqlKey ctId
  courseDetails = courseDetailsFromModel e userDetails
  in CourseTaskDetails' (fromIntegral taskId) courseTaskName courseTaskContent courseTaskOrderNumber courseDetails accepted

courseTaskDetailFromModels :: Entity CourseTask -> Entity Course -> Maybe UserDetails -> Either String CourseTaskDetails
courseTaskDetailFromModels (Entity ctId (CourseTask { .. })) e userDetails = let
  taskId = fromSqlKey ctId
  courseDetails = courseDetailsFromModel e userDetails
  standActions = case (eitherDecode . fromStrict) courseTaskStandActions :: Either String [StandCheckStage] of
                  (Left e') -> Left $ "standActions: " <> e'
                  (Right r) -> Right r
  in do
    actions' <- standActions
    return $  CourseTaskDetails (fromIntegral taskId) courseTaskName courseTaskContent courseTaskOrderNumber courseDetails courseTaskStandIdentifier actions'
