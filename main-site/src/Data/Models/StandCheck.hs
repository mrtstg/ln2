{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Models.StandCheck (StandCheckStage(..), convertStandCheckList) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap as K
import qualified Data.Text         as T
import           GHC.Generics
import           Utils             (unsafeRandomString)

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
  | CopyAnswer
  { getStageContainer :: !ContainerName
  , getStageFilePath  :: !FilePath
  }
  | CompareResults
  { getFirstCompareV  :: !T.Text
  , getSecondCompareV :: !T.Text
  , getCompareScore   :: !Int
  }
  | DeclareVariable
  { getStandDeclaringVariable :: !T.Text
  , getStandDeclaredValue     :: !Value
  }
  | PSQLExists
  { getStageContainer :: !ContainerName
  , getCheckQuery     :: !T.Text
  , getCheckScore     :: !Int
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
  toJSON (DeclareVariable varname varvalue) = object
    [ "action" .= String "declare"
    , "variableName" .= String varname
    , "variableValue" .= varvalue
    ]
  toJSON (PSQLExists container query score) = object
    [ "action" .= String "psql_exists_macro"
    , "container" .= container
    , "query" .= String query
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
      <*> v .:? "recordInto"
    (Just (String "copyAnswer")) -> CopyAnswer
      <$> v .: "container"
      <*> v .: "filePath"
    (Just (String "compareVars")) -> CompareResults
      <$> v .: "first"
      <*> v .: "second"
      <*> v .: "score"
    (Just (String "declare")) -> DeclareVariable
      <$> v .: "variableName"
      <*> v .: "variableValue"
    (Just (String "psql_exists_macro")) -> PSQLExists
      <$> v .: "container"
      <*> v .: "query"
      <*> v .: "score"
    _anyOther -> fail "Wrong action type, excepted string!"

-- заменяет "особый" блок ответа на копирование файла
convertStandCheckList :: T.Text -> [StandCheckStage] -> [StandCheckStage]
convertStandCheckList answer = foldr f [] where
  f :: StandCheckStage -> [StandCheckStage] -> [StandCheckStage]
  f (CopyAnswer container filePath) acc = CopyFile container answer filePath:acc
  f (PSQLExists container query score) acc  =
    [ CopyFile container ("select (CASE WHEN EXISTS(" <> query <> ") THEN 1 ELSE 0 END);") scriptName
    , ExecuteCommand container ("psql -f " <> scriptName' <> " --csv -t") False True (Just scriptName')
    , DeclareVariable (scriptName' <> "-correct") (String "1")
    , CompareResults (scriptName' <> "-correct") scriptName' score
    ] ++ acc where
      scriptName = "/" <> unsafeRandomString 15 <> ".sql"
      scriptName' = T.pack scriptName
  f other acc                           = other:acc
