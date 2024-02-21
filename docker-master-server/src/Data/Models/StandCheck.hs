{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Models.StandCheck (StandCheckStage(..)) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap as K
import qualified Data.Text         as T
import           GHC.Generics

type ContainerName = String

data StandCheckStage = CopyFile
  { getStageContainer   :: !ContainerName
  , getStageFileContent :: !T.Text
  , getStageFilePath    :: !FilePath
  }
  | ExecuteCommand
  { getStageContainer    :: !ContainerName
  , getStageCommand      :: !T.Text
  , getStandRecordStdout :: !Bool
  } deriving (Show, Eq, Generic)

instance ToJSON StandCheckStage where
  toJSON (CopyFile container file path) = object ["action" .= String "copy", "container" .= container, "fileContent" .= file, "filePath" .= path]
  toJSON (ExecuteCommand container command recordStdout) = object ["action" .= String "command", "container" .= container, "command" .= command, "recordStdout" .= recordStdout]

instance FromJSON StandCheckStage where
  parseJSON = withObject "StandCheckStage" $ \v -> case K.lookup "action" v of
    Nothing -> fail "No action specified"
    (Just (String "copy")) -> CopyFile <$> v .: "container" <*> v .: "fileContent" <*> v .: "filePath"
    (Just (String "command")) -> ExecuteCommand <$> v .: "container" <*> v .: "command" <*> v .: "recordStdout"
    _anyOther -> fail "Wrong action type, excepted string!"
