{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.UserPatch
  ( UserPatch(..)
  , userPatchToQuery
  ) where

import           Data.Aeson
import           Data.Text
import           Database.Persist
import           Foundation
import           Utils            (sha256Text)

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

userPatchToQuery :: UserPatch -> [Update User]
userPatchToQuery (UserPatch { .. }) = let
  loginQ Nothing       = []
  loginQ (Just nLogin) = [UserLogin =. nLogin]
  nameQ Nothing      = []
  nameQ (Just nName) = [UserName =. nName]
  passwordQ Nothing      = []
  passwordQ (Just nPass) = [UserPasswordHash =. sha256Text nPass]
  in loginQ getUserPatchLogin
    ++ nameQ getUserPatchName
    ++ passwordQ getUserPatchPassword
