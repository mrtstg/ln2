{-# LANGUAGE OverloadedStrings #-}
module Data.Models.UserSearch
  ( UserSearch(..)
  ) where

import           Data.Aeson
import           Data.Text  (Text)

data UserSearch = UserSearch
  { getUserSearchQuery        :: !Text
  , getUserSearchGroup        :: !Text
  , getUserSearchExcludeGroup :: !Text
  , getUserSearchPage         :: !Int
  } deriving Show

instance FromJSON UserSearch where
  parseJSON = withObject "UserSearch" $ \v -> UserSearch
    <$> v .: "query"
    <*> v .:? "group" .!= ""
    <*> v .:? "excludeGroup" .!= ""
    <*> v .:? "page" .!= 1
