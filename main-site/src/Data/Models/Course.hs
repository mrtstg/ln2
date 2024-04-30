{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Course
  ( CourseCreate(..)
  , CourseDetails(..)
  , courseDetailsFromModel
  ) where

import           Data.Aeson
import           Data.Models.User
import           Data.Text        (Text, unpack)
import           Data.Time.Clock
import           Database.Persist
import           Foundation

data CourseCreate = CourseCreate
  { getCourseCreateName        :: !Text
  , getCourseCreateDescription :: !Text
  } deriving Show

instance FromJSON CourseCreate where
  parseJSON = withObject "CourseCreate" $ \v -> CourseCreate
    <$> v .: "name"
    <*> v .: "description"

data CourseDetails = CourseDetails
  { getCourseDetailsId          :: !String
  , getCourseDetailsName        :: !Text
  , getCourseDetailsDescription :: !Text
  , getCourseDetailsAuthorId    :: !Int
  , getCourseDetailsAuthor      :: !(Maybe UserDetails)
  , getCourseDetailsCreatedAt   :: !UTCTime
  }

instance ToJSON CourseDetails where
  toJSON (CourseDetails { .. }) = object
    [ "id" .= getCourseDetailsId
    , "name" .= getCourseDetailsName
    , "description" .= getCourseDetailsDescription
    , "authorId" .= getCourseDetailsAuthorId
    , "author" .= getCourseDetailsAuthor
    , "createdAt" .= getCourseDetailsCreatedAt
    ]

courseDetailsFromModel :: Entity Course -> Maybe UserDetails -> CourseDetails
courseDetailsFromModel
  (Entity (CourseKey cId) (Course cName cDesc cAuthorId cCreatedAt))
  userDetails = CourseDetails cId cName cDesc cAuthorId userDetails cCreatedAt
