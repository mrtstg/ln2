{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Models.StandCheck (StandCheckStage(..), convertStandCheckList) where

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
  { getStageContainer       :: !ContainerName
  , getStageCommand         :: !T.Text
  , getStandRecordStdout    :: !Bool
  , getStandFormattedOutput :: !Bool
  }
  | CopyAnswer
  { getStageContainer :: !ContainerName
  , getStageFilePath  :: !FilePath
  }
  | CompareResults
  { getFirstCompareV  :: !T.Text
  , getSecondCompareV :: !T.Text
  , getCompareScore   :: !Int
  } deriving (Show, Eq, Generic)

instance ToJSON StandCheckStage where
  toJSON (CopyFile container file path) = object ["action" .= String "copy", "container" .= container, "fileContent" .= file, "filePath" .= path]
  toJSON (ExecuteCommand container command recordStdout formatOut) = object
    [ "action" .= String "command"
    , "container" .= container
    , "command" .= command
    , "recordStdout" .= recordStdout
    , "formatOutput" .= formatOut
    ]
  toJSON (CopyAnswer container path) = object
    [ "action" .= String "copyAnswer"
    , "container" .= container
    , "filePath" .= path
    ]
  toJSON (CompareResults fvar svar score) = object
    [ "action" .= String "compareVars"
    , "first" .= String fvar
    , "second" .= String svar
    , "score" .= score
    ]

instance FromJSON StandCheckStage where
  parseJSON = withObject "StandCheckStage" $ \v -> case K.lookup "action" v of
    Nothing -> fail "No action specified"
    (Just (String "copy")) -> CopyFile <$> v .: "container" <*> v .: "fileContent" <*> v .: "filePath"
    (Just (String "command")) -> ExecuteCommand
      <$> v .: "container"
      <*> v .: "command"
      <*> v .:? "recordStdout" .!= True
      <*> v .:? "formatOutput" .!= False
    (Just (String "copyAnswer")) -> CopyAnswer
      <$> v .: "container"
      <*> v .: "filePath"
    (Just (String "compareVars")) -> CompareResults
      <$> v .: "first"
      <*> v .: "second"
      <*> v .: "score"
    _anyOther -> fail "Wrong action type, excepted string!"

-- заменяет "особый" блок ответа на копирование файла
convertStandCheckList :: T.Text -> [StandCheckStage] -> [StandCheckStage]
convertStandCheckList answer = map f where
  f :: StandCheckStage -> StandCheckStage
  f (CopyAnswer container filePath) = CopyFile container answer filePath
  f other                           = other
