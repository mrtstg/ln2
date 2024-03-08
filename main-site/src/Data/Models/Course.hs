{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Course
  ( CourseCreate(..)
  , CourseDetails(..)
  , courseDetailsFromModel
  ) where

import           Data.Aeson
import           Data.Models.User
import           Data.Text        (unpack)
import           Data.Time.Clock
import           Database.Persist
import           Foundation

newtype CourseCreate = CourseCreate
  { getCourseCreateName :: String
  } deriving Show

instance FromJSON CourseCreate where
  parseJSON = withObject "CourseCreate" $ \v ->
    CourseCreate <$>
    v .: "name"

data CourseDetails = CourseDetails
  { getCourseDetailsId        :: !String
  , getCourseDetailsName      :: !String
  , getCourseDetailsAuthorId  :: !Int
  , getCourseDetailsAuthor    :: !(Maybe UserDetails)
  , getCourseDetailsCreatedAt :: !UTCTime
  }

instance ToJSON CourseDetails where
  toJSON (CourseDetails { .. }) = object
    [ "id" .= getCourseDetailsId
    , "name" .= getCourseDetailsName
    , "authorId" .= getCourseDetailsAuthorId
    , "author" .= getCourseDetailsAuthor
    , "createdAt" .= getCourseDetailsCreatedAt
    ]

courseDetailsFromModel :: Entity Course -> Maybe UserDetails -> CourseDetails
courseDetailsFromModel
  (Entity (CourseKey cId) (Course cName cAuthorId cCreatedAt))
  userDetails = CourseDetails cId (unpack cName) cAuthorId userDetails cCreatedAt
