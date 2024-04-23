{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.User (UserDetails(..), userDetailsFromModel) where

import           Data.Aeson
import           Data.Models.Role
import           Data.Text            (Text)
import           Database.Persist
import           Database.Persist.Sql
import           Foundation
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

userDetailsFromModel :: Entity User -> [Entity Role] -> UserDetails
userDetailsFromModel e userRoles = let
  (User login _) = entityVal e
  userKey = (fromSqlKey . entityKey) e
  in UserDetails (fromIntegral userKey) login (map (\(Entity _ r) -> roleDetailsFromModel r) userRoles)
