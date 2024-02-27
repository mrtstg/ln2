{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.User (UserDetails(..), userDetailsFromModel) where

import           Data.Aeson
import           Data.Text
import           Database.Persist
import           Database.Persist.Sql
import           Foundation
import           GHC.Generics

data UserDetails = UserDetails
  { getUserDetailsId   :: Int
  , getUserDetailsName :: Text
  } deriving (Show, Generic)

instance ToJSON UserDetails where
  toJSON (UserDetails { .. }) = object
    [ "id" .= getUserDetailsId
    , "login" .= getUserDetailsName
    ]

userDetailsFromModel :: Entity User -> UserDetails
userDetailsFromModel e = let
  (User login _) = entityVal e
  userKey = (fromSqlKey . entityKey) e
  in UserDetails (fromIntegral userKey) login
