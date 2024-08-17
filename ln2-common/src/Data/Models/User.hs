{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.User
  ( UserDetails(..)
  , UserCreate(..)
  ) where

import           Data.Aeson
import           Data.Models.Auth.Role
import           Data.Text             (Text)
import           GHC.Generics

data UserCreate = UserCreate
  { getUserCreateLogin    :: !Text
  , getUserCreateName     :: !Text
  , getUserCreatePassword :: !Text
  } deriving (Show)

instance FromJSON UserCreate where
  parseJSON = withObject "UserCreate" $ \v -> UserCreate
    <$> v .: "login"
    <*> v .: "name"
    <*> v .: "password"

instance ToJSON UserCreate where
  toJSON (UserCreate { .. }) = object
    [ "login" .= getUserCreateLogin
    , "name" .= getUserCreateName
    , "password" .= getUserCreatePassword
    ]

data UserDetails = UserDetails
  { getUserDetailsId    :: !Int
  , getUserDetailsLogin :: !Text
  , getUserDetailsName  :: !Text
  , getUserRoles        :: ![RoleDetails]
  } deriving (Show, Generic)

instance ToJSON UserDetails where
  toJSON (UserDetails { .. }) = object
    [ "id" .= getUserDetailsId
    , "login" .= getUserDetailsLogin
    , "name" .= getUserDetailsName
    , "roles" .= getUserRoles
    ]

instance FromJSON UserDetails where
  parseJSON = withObject "UserDetails" $ \v -> UserDetails
    <$> v .: "id"
    <*> v .: "login"
    <*> v .: "name"
    <*> v .: "roles"
