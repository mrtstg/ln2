{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Auth.Role
  ( RoleDetails(..)
  ) where

import           Data.Aeson
import           Data.Text  (Text)

data RoleDetails = RoleDetails
  { getRoleName        :: !Text
  , getRoleDisplayName :: !Text
  } deriving (Eq, Show)

instance ToJSON RoleDetails where
  toJSON (RoleDetails { .. }) = object
    [ "name" .= getRoleName
    , "displayName" .= getRoleDisplayName
    ]

instance FromJSON RoleDetails where
  parseJSON = withObject "RoleDetails" $ \v -> RoleDetails
    <$> v .: "name"
    <*> v .: "displayName"
