{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.User (UserDetails(..)) where

import           Data.Aeson
import           Data.Models.Role
import           Data.Text        (Text)
import           GHC.Generics

data UserDetails = UserDetails
  { getUserDetailsId    :: !Int
  , getUserDetailsLogin :: !Text
  , getUserRoles        :: ![RoleDetails]
  } deriving (Show, Generic)

instance ToJSON UserDetails where
  toJSON (UserDetails { .. }) = object
    [ "id" .= getUserDetailsId
    , "login" .= getUserDetailsLogin
    , "roles" .= getUserRoles
    ]

instance FromJSON UserDetails where
  parseJSON = withObject "UserDetails" $ \v -> UserDetails
    <$> v .: "id"
    <*> v .: "login"
    <*> v .: "roles"
