{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.CheckMessage
  ( CheckMessage(..)
  , CheckMessageBlock(..)
  , CheckBlockType(..)
  ) where

import           Data.Aeson
import           Data.Text  (Text)

data CheckBlockType = Code | Message deriving Show

instance ToJSON CheckBlockType where
  toJSON Code    = String "code"
  toJSON Message = String "message"

instance FromJSON CheckBlockType where
  parseJSON = withText "CheckBlockType" $ \case
    "code" -> pure Code
    "message" -> pure Message
    _ -> fail "Invalid CheckBlockType!"

data CheckMessageBlock = CheckMessageBlock
  { getBlockType    :: !CheckBlockType
  , getBlockContent :: !Text
  } deriving Show

instance ToJSON CheckMessageBlock where
  toJSON (CheckMessageBlock { .. }) = object [ "type" .= getBlockType, "content" .= getBlockContent ]

instance FromJSON CheckMessageBlock where
  parseJSON = withObject "CheckMessageBlock" $ \v -> CheckMessageBlock
    <$> v .: "type"
    <*> v .: "content"

data CheckMessage = CheckMessage
  { getMessageTitle  :: !Text
  , getMessageBlocks :: ![CheckMessageBlock]
  } deriving Show

instance ToJSON CheckMessage where
  toJSON (CheckMessage { .. }) = object [ "title" .= getMessageTitle, "blocks" .= getMessageBlocks ]

instance FromJSON CheckMessage where
  parseJSON = withObject "CheckMessage" $ \v -> CheckMessage
    <$> v .: "title"
    <*> v .: "blocks"
