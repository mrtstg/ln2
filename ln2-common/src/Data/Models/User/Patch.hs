{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.User.Patch
  ( UserPatch(..)
  ) where

import           Data.Aeson
import           Data.Text

data UserPatch = UserPatch
  { getUserPatchLogin    :: !(Maybe Text)
  , getUserPatchName     :: !(Maybe Text)
  , getUserPatchPassword :: !(Maybe Text)
  } deriving (Show)

instance FromJSON UserPatch where
  parseJSON = withObject "UserPatch" $ \v -> UserPatch
    <$> v .:? "login"
    <*> v .:? "name"
    <*> v .:? "password"

instance ToJSON UserPatch where
  toJSON (UserPatch { .. }) = object
    [ "login" .= getUserPatchLogin
    , "name" .= getUserPatchName
    , "password" .= getUserPatchPassword
    ]
