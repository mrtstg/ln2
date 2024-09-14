{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Auth.Token
  ( AuthTokenRequest(..)
  , AuthTokenResponse(..)
  ) where

import           Data.Aeson
import           Data.Text  (Text)

newtype AuthTokenRequest = AuthTokenRequest String deriving Show

instance ToJSON AuthTokenRequest where
  toJSON (AuthTokenRequest token) = object
    [ "error" .= token
    ]

newtype AuthTokenResponse = AuthTokenResponse
  { getTokenResponseService :: Text
  } deriving Show

instance ToJSON AuthTokenResponse where
  toJSON (AuthTokenResponse { .. }) = object
    [ "service" .= getTokenResponseService
    ]

instance FromJSON AuthTokenResponse where
  parseJSON = withObject "AuthTokenResponse" $ \v -> AuthTokenResponse
    <$> v .: "service"
