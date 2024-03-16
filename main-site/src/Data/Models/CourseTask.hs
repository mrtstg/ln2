{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.CourseTask
  ( CourseTaskCreate(..)
  , CourseTaskDetails(..)
  , CourseTaskDetails'(..)
  , courseTaskDetailFromModels
  , courseTaskDetailFromModels'
  , courseTaskWithSolveFromModel
  ) where

import           Data.Aeson
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Models.Course
import           Data.Models.StandCheck
import           Data.Models.User
import           Data.Text
import           Database.Persist
import           Database.Persist.Sql
import           Foundation

data CourseTaskCreate = CourseTaskCreate
  { getCourseTaskCreateName            :: !Text
  , getCourseTaskCreateContent         :: !Text
  , getCourseTaskCreateOrder           :: !Int
  , getCourseTaskCreateStandIdentifier :: !Text
  , getCourseTaskCreateStandActions    :: ![StandCheckStage]
  , getCourseTaskCreateAwaitedRes      :: !Value
  }

instance FromJSON CourseTaskCreate where
  parseJSON = withObject "CourseTaskCreate" $ \v -> CourseTaskCreate
    <$> v .: "name"
    <*> v .: "content"
    <*> v .:? "order" .!= 0
    <*> v .: "standIdentifier"
    <*> v .: "standActions"
    <*> v .: "awaitedResult"

data CourseTaskDetails = CourseTaskDetails
  { getCourseTaskDId           :: !Int
  , getCourseTaskDName         :: !Text
  , getCourseTaskDContent      :: !Text
  , getCourseTaskDOrder        :: !Int
  , getCourseTaskDCourse       :: !CourseDetails
  , getCourseTaskDStand        :: !Text
  , getCourseTaskDStandActions :: ![StandCheckStage]
  , getCourseTaskDAwaitedRes   :: !Value
  }

data CourseTaskDetails' = CourseTaskDetails'
  { getCourseTaskDId'      :: !Int
  , getCourseTaskDName'    :: !Text
  , getCourseTaskDContent' :: !Text
  , getCourseTaskDOrder'   :: !Int
  , getCourseTaskDCourse'  :: !CourseDetails
  }

data CourseTaskSolve = CourseTaskSolve
  { getCourseTaskSolveId      :: !Text
  , getCourseTaskSolveCorrect :: !Bool
  }

instance ToJSON CourseTaskSolve where
  toJSON (CourseTaskSolve { .. }) = object
    [ "id" .= getCourseTaskSolveId
    , "correct" .= getCourseTaskSolveCorrect
    ]

data CourseTaskWithSolves = CourseTaskWithSolves
  { getCourseTaskWSId       :: !Int
  , getCourseTaskWSName     :: !Text
  , getCourseTaskWSContent  :: !Text
  , getCourseTaskWSCourse   :: !CourseDetails
  , getCourseTaskWSAccepted :: !Bool
  , getCourseTaskWSSolves   :: ![CourseTaskSolve]
  }

instance ToJSON CourseTaskWithSolves where
  toJSON (CourseTaskWithSolves { .. }) = object
    [ "id" .= getCourseTaskWSId
    , "name" .= getCourseTaskWSName
    , "content" .= getCourseTaskWSContent
    , "course" .= getCourseTaskWSCourse
    , "accepted" .= getCourseTaskWSAccepted
    , "solves" .= getCourseTaskWSSolves
    ]

courseTaskWithSolveFromModel :: Entity CourseTask -> Entity Course -> [Entity CourseSolves] -> Bool -> CourseTaskWithSolves
courseTaskWithSolveFromModel (Entity ctId (CourseTask { .. })) e solves accepted =
   CourseTaskWithSolves
    (fromIntegral taskId)
    courseTaskName
    courseTaskContent
    courseDetails
    accepted
    solves' where
      solves' = Prelude.map (\(Entity (CourseSolvesKey solveId) (CourseSolves { .. })) -> CourseTaskSolve (pack solveId) courseSolvesCorrect) solves
      taskId = fromSqlKey ctId
      courseDetails = courseDetailsFromModel e Nothing

instance ToJSON CourseTaskDetails' where
  toJSON (CourseTaskDetails' { .. }) = object
    [ "id" .= getCourseTaskDId'
    , "name" .= getCourseTaskDName'
    , "content" .= getCourseTaskDContent'
    , "order" .= getCourseTaskDOrder'
    , "course" .= getCourseTaskDCourse'
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
    , "awaitedResult" .= getCourseTaskDAwaitedRes
    ]

courseTaskDetailFromModels' :: Entity CourseTask -> Entity Course -> Maybe UserDetails -> CourseTaskDetails'
courseTaskDetailFromModels' (Entity ctId (CourseTask { .. })) e userDetails = let
  taskId = fromSqlKey ctId
  courseDetails = courseDetailsFromModel e userDetails
  in CourseTaskDetails' (fromIntegral taskId) courseTaskName courseTaskContent courseTaskOrderNumber courseDetails

courseTaskDetailFromModels :: Entity CourseTask -> Entity Course -> Maybe UserDetails -> Either String CourseTaskDetails
courseTaskDetailFromModels (Entity ctId (CourseTask { .. })) e userDetails = let
  taskId = fromSqlKey ctId
  courseDetails = courseDetailsFromModel e userDetails
  standActions = case (eitherDecode . fromStrict) courseTaskStandActions :: Either String [StandCheckStage] of
                  (Left e') -> Left $ "standActions: " <> e'
                  (Right r) -> Right r
  awaitedResult = case (eitherDecode . fromStrict) courseTaskAwaitedResult :: Either String Value of
                    (Left e') -> Left $ "awaitedResult: " <> e'
                    (Right r) -> Right r
  in do
    actions' <- standActions
    res' <- awaitedResult
    return $  CourseTaskDetails (fromIntegral taskId) courseTaskName courseTaskContent courseTaskOrderNumber courseDetails courseTaskStandIdentifier actions' res'
