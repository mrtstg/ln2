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
  { getStageContainer       :: !ContainerName
  , getStageCommand         :: !T.Text
  , getStandRecordStdout    :: !Bool
  , getStandFormattedOutput :: !Bool
  , getStandRecordVariable  :: !(Maybe T.Text)
  }
  | CompareResults
  { getFirstCompareV  :: !T.Text
  , getSecondCompareV :: !T.Text
  , getCompareScore   :: !Int
  }
  | DeclareVariable
  { getStandDeclaringVariable :: !T.Text
  , getStandDeclaredValue     :: !Value
  } deriving (Show, Eq, Generic)

instance ToJSON StandCheckStage where
  toJSON (CopyFile container file path) = object ["action" .= String "copy", "container" .= container, "fileContent" .= file, "filePath" .= path]
  toJSON (ExecuteCommand container command recordStdout formatOut recordVar) = object
    [ "action" .= String "command"
    , "container" .= container
    , "command" .= command
    , "recordStdout" .= recordStdout
    , "formatOutput" .= formatOut
    , "recordInto" .= recordVar
    ]
  toJSON (CompareResults fvar svar score) = object
    [ "action" .= String "compareVars"
    , "first" .= String fvar
    , "second" .= String svar
    , "score" .= score
    ]
  toJSON (DeclareVariable varname varvalue) = object
    [ "action" .= String "declare"
    , "variableName" .= String varname
    , "variableValue" .= varvalue
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
      <*> v .:? "recordInto"
    (Just (String "compareVars")) -> CompareResults
      <$> v .: "first"
      <*> v .: "second"
      <*> v .: "score"
    (Just (String "declare")) -> DeclareVariable
      <$> v .: "variableName"
      <*> v .: "variableValue"
    _anyOther -> fail "Wrong action type, excepted string!"
