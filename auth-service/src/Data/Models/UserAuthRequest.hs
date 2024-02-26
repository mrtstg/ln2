{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Models.UserAuthRequest (UserAuthRequest(..)) where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

data UserAuthRequest = UserAuthRequest
  { getAuthRequestLogin    :: !Text
  , getAuthRequestPassword :: !Text
  } deriving (Eq, Show, Generic)

instance FromJSON UserAuthRequest where
  parseJSON = withObject "UserAuthRequest" $ \v -> UserAuthRequest
    <$> v .: "login"
    <*> v .: "password"
