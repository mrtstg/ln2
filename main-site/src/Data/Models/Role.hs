{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Role
  ( RoleDetails(..)
  , adminRoleGranted
  ) where

import           Data.Aeson
import           Data.Text  (Text)

data RoleDetails = RoleDetails
  { getRoleName        :: !Text
  , getRoleDisplayName :: !Text
  } deriving Show

instance ToJSON RoleDetails where
  toJSON (RoleDetails { .. }) = object
    [ "name" .= getRoleName
    , "displayName" .= getRoleDisplayName
    ]

instance FromJSON RoleDetails where
  parseJSON = withObject "RoleDetails" $ \v -> RoleDetails
    <$> v .: "name"
    <*> v .: "displayName"

adminRoleGranted :: [RoleDetails] -> Bool
adminRoleGranted = any (\(RoleDetails name _) -> name == "admins")
