{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Role
  ( RoleDetails(..)
  , roleDetailsFromModel
  ) where

import           Data.Aeson
import           Data.Text  (Text)
import           Foundation

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

roleDetailsFromModel :: Role -> RoleDetails
roleDetailsFromModel (Role name dName) = RoleDetails name dName
