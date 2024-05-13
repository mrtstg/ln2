{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Models.StandCheck (StandCheckStage(..), convertStandCheckList) where

import           Api.DBApi
import           Data.Aeson
import qualified Data.Aeson.KeyMap     as K
import           Data.Either
import           Data.Models.Database
import           Data.Models.Endpoints (EndpointsConfiguration)
import qualified Data.Text             as T
import           Utils                 (unsafeRandomString)

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
  }
  | PSQLQuery
  { getStageContainer      :: !ContainerName
  , getCheckQuery          :: !T.Text
  , getStandRecordVariable :: !(Maybe T.Text)
  }
  | PSQLAnswerQuery
  { getStageContainer      :: !ContainerName
  , getStandRecordVariable :: !(Maybe T.Text)
  }
  | PSQLGenerateDatabase
  { getStageContainer :: !ContainerName
  , getDatabaseInfo   :: !DatabaseData
  }deriving Show

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
  toJSON (PSQLQuery container query recordInto) = object
    [ "action" .= String "psql_query_macro"
    , "container" .= container
    , "query" .= query
    , "recordInto" .= recordInto
    ]
  toJSON (PSQLAnswerQuery container recordInto) = object
    [ "action" .= String "psql_answer_query_macro"
    , "container" .= container
    , "recordInto" .= recordInto]
  toJSON (PSQLGenerateDatabase container db) = object
    [ "action" .= String "psql_generate_database"
    , "container" .= container
    , "database" .= db
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
    (Just (String "psql_query_macro")) -> PSQLQuery
      <$> v .: "container"
      <*> v .: "query"
      <*> v .:? "recordInto"
    (Just (String "psql_answer_query_macro")) -> PSQLAnswerQuery
      <$> v .: "container"
      <*> v .:? "recordInto"
    (Just (String "psql_generate_database")) -> PSQLGenerateDatabase
      <$> v .: "container"
      <*> v .: "database"
    _anyOther -> fail "Wrong action type, excepted string!"

-- развертка макросов в простые составные блоки
convertStandCheckList :: EndpointsConfiguration -> T.Text -> [StandCheckStage] -> IO (Either String [StandCheckStage])
convertStandCheckList endpoints answer stages = do
  res <- mapM f stages
  if any isLeft res then return (head $ filter isLeft res) else do
    (return . Right) $ concatMap (fromRight []) res
  where
  f :: StandCheckStage -> IO (Either String [StandCheckStage])
  f (PSQLGenerateDatabase container db) = do
    resp <- convertCreateDB' endpoints db
    case resp of
      (DBApiResult query) -> return $ return
        [ CopyFile container query "/db.sql"
        , ExecuteCommand container "psql -f /db.sql" False True (Just "db-creation")
        ]
      (DBApiError err) -> return $ Left ("Ошибка проверки БД: " <> T.unpack err)
      _anyOther -> return $ Left "Неизвестная ошибка со стороны проверки БД"
  f (CopyAnswer container filePath) = return $ return [CopyFile container answer filePath]
  f (PSQLExists container query score) = return $ return
    [ CopyFile container ("select (CASE WHEN EXISTS(" <> query' <> ") THEN 1 ELSE 0 END);") scriptName
    , ExecuteCommand container ("psql -f " <> scriptName' <> " --csv -t") False True (Just scriptName')
    , DeclareVariable (scriptName' <> "-correct") (String "1")
    , CompareResults (scriptName' <> "-correct") scriptName' score
    ] where
      query' = if T.last query == ';' then T.init query else query
      scriptName = "/" <> unsafeRandomString 15 <> ".sql"
      scriptName' = T.pack scriptName
  f (PSQLQuery container query recordInto) = return $ return
    [ CopyFile container query scriptName
    , ExecuteCommand container ("psql -f " <> scriptName' <> " --csv") False True recordInto
    ] where
      scriptName = "/" <> unsafeRandomString 14 <> ".sql"
      scriptName' = T.pack scriptName
  f (PSQLAnswerQuery container recordInto) = return $ return
    [ CopyFile container answer scriptName
    , ExecuteCommand container ("psql -f " <> scriptName' <> " --csv") False True recordInto
    ] where
      scriptName = "/" <> unsafeRandomString 14 <> ".sql"
      scriptName' = T.pack scriptName
  f other                          = return $ return [other]
