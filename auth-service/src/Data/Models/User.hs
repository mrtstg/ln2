{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.User (UserDetails(..), userDetailsFromModel, UserCreate(..)) where

import           Data.Aeson
import           Data.Models.Role
import           Data.Text            (Text)
import           Database.Persist
import           Database.Persist.Sql
import           Foundation
import           GHC.Generics

data UserCreate = UserCreate
  { getUserCreateLogin    :: !Text
  , getUserCreateName     :: !Text
  , getUserCreatePassword :: !Text
  , getUserCreateRoles    :: ![Text]
  } deriving (Show)

instance FromJSON UserCreate where
  parseJSON = withObject "UserCreate" $ \v -> UserCreate
    <$> v .: "login"
    <*> v .: "name"
    <*> v .: "password"
    <*> v .:? "roles" .!= []

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

userDetailsFromModel :: Entity User -> [Entity Role] -> UserDetails
userDetailsFromModel e userRoles = let
  (User { .. }) = entityVal e
  userKey = (fromSqlKey . entityKey) e
  in UserDetails (fromIntegral userKey) userLogin userName (map (\(Entity _ r) -> roleDetailsFromModel r) userRoles)
