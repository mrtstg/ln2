{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.UserQuery (UserQuery(..)) where

import           Data.Aeson
import           Data.Models.User

data UserQuery = UserQuery
  { getUserQueryTotal    :: !Int
  , getUserQueryPageSize :: !Int
  , getUserQueryUsers    :: ![UserDetails]
  } deriving (Show)

instance FromJSON UserQuery where
  parseJSON = withObject "UserQuery" $ \v -> UserQuery
    <$> v .: "total"
    <*> v .: "pageSize"
    <*> v .: "objects"

instance ToJSON UserQuery where
  toJSON (UserQuery { .. }) = object
    [ "total" .= getUserQueryTotal
    , "pageSize" .= getUserQueryPageSize
    , "objects" .= getUserQueryUsers
    ]
