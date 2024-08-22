{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Models.StandCheck (StandCheckStage (..), convertStandCheckList) where

import           Api.DBApi
import           Data.Aeson
import qualified Data.Aeson.KeyMap     as K
import           Data.Either
import           Data.Models.Database
import           Data.Models.Endpoints (EndpointsConfiguration)
import qualified Data.Text             as T
import           System.Random         (StdGen)
import           Utils                 (unsafeRandomString)

type StageTarget = String

data StandCheckStage
  = CopyFile
  { getStageTarget      :: !StageTarget
  , getStageFileContent :: !T.Text
  , getStageFilePath    :: !FilePath
  }
  | ExecuteCommand
  { getStageTarget         :: !StageTarget
  , getStageCommand        :: !T.Text
  , getStageFormatOutput   :: !Bool
  , getStageRecordVariable :: !(Maybe T.Text)
  , getStageReportError    :: !Bool
  }
  | AddPoints
  { getStagePointsAmount :: !Int
  }
  | CompareVariables
  { getStageFirstV          :: !T.Text
  , getStageSecondV         :: !T.Text
  , getStagePositiveActions :: ![StandCheckStage]
  , getStageNegativeActions :: ![StandCheckStage]
  }
  | CompareLatestStatusCode
  { getAwaitedStatus        :: !Int
  , getStagePositiveActions :: ![StandCheckStage]
  , getStageNegativeActions :: ![StandCheckStage]
  }
  | DeclareVariable
  { getStageVariableName  :: !T.Text
  , getStageVariableValue :: !T.Text
  }
  | DisplayMessage
  { getStageMessage      :: !T.Text
  , getStageMessageTitle :: !T.Text
  }
  | DisplayVariable
  { getStageVariableName :: !T.Text
  , getStageMessage      :: !T.Text
  , getStageMessageTitle :: !T.Text
  }
  | StopCheck {}
  | AcceptCheck {}
  | SetPointsGate
  { getStageNeededPoints :: !Int
  }
  | PSQLExists
  { getStageTarget          :: !StageTarget
  , getStageQuery           :: !T.Text
  , getStagePositiveActions :: ![StandCheckStage]
  , getStageNegativeActions :: ![StandCheckStage]
  }
  | PSQLQuery
  { getStageTarget         :: !StageTarget
  , getStageQuery          :: !T.Text
  , getStageRecordVariable :: !(Maybe T.Text)
  }
  | PSQLAnswerQuery
  { getStageTarget         :: !StageTarget
  , getStageRecordVariable :: !(Maybe T.Text)
  }
  | PSQLGenerateDatabase
  { getStageTarget       :: !StageTarget
  , getStageDatabaseInfo :: !DatabaseData
  }
  | PSQLTableExists
  { getStageTarget          :: !StageTarget
  , getStageDatabaseSchema  :: !T.Text
  , getStageTableName       :: !T.Text
  , getStagePositiveActions :: ![StandCheckStage]
  , getStageNegativeActions :: ![StandCheckStage]
  }
  | PSQLColumnTypeCheck
  { getStageTarget          :: !StageTarget
  , getStageDatabaseSchema  :: !T.Text
  , getStageTableName       :: !T.Text
  , getStageColumnName      :: !T.Text
  , getStageAwaitedType     :: !T.Text
  , getStagePositiveActions :: ![StandCheckStage]
  , getStageNegativeActions :: ![StandCheckStage]
  }
  | CopyAnswer
  { getStageTarget   :: !StageTarget
  , getStageFilePath :: !FilePath
  }
  deriving (Show)

instance ToJSON StandCheckStage where
  toJSON (CopyFile { .. }) = object
    [ "action" .= String "copy"
    , "target" .= getStageTarget
    , "content" .= getStageFileContent
    , "path" .= getStageFilePath
    ]
  toJSON (ExecuteCommand { .. }) = object
    [ "action" .= String "command"
    , "target" .= getStageTarget
    , "command" .= getStageCommand
    , "formatOutput" .= getStageFormatOutput
    , "recordInto" .= getStageRecordVariable
    , "reportError" .= getStageReportError
    ]
  toJSON (AddPoints { .. }) = object
    [ "action" .= String "points"
    , "amount" .= getStagePointsAmount
    ]
  toJSON (CompareVariables { .. }) = object
    [ "action" .= String "compareVars"
    , "first" .= getStageFirstV
    , "second" .= getStageSecondV
    , "positiveActions" .= getStagePositiveActions
    , "negativeActions" .= getStageNegativeActions
    ]
  toJSON (CompareLatestStatusCode { .. }) = object
    [ "action" .= String "compareStatusCode"
    , "awaitedStatus" .= getAwaitedStatus
    , "positiveActions" .= getStagePositiveActions
    , "negativeActions" .= getStageNegativeActions
    ]
  toJSON (DeclareVariable { .. }) = object
    [ "action" .= String "declare"
    , "variableName" .= getStageVariableName
    , "variableValue" .= getStageVariableValue
    ]
  toJSON StopCheck = object
    [ "action" .= String "stopCheck"
    ]
  toJSON AcceptCheck = object
    [ "action" .= String "acceptCheck"
    ]
  toJSON (DisplayMessage { .. }) = object
    [ "action" .= String "displayMessage"
    , "message" .= getStageMessage
    , "title" .= getStageMessageTitle
    ]
  toJSON (DisplayVariable { .. }) = object
    [ "action" .= String "displayVariable"
    , "variableName" .= getStageVariableName
    , "message" .= getStageMessage
    , "title" .= getStageMessageTitle
    ]
  toJSON (SetPointsGate { .. }) = object
    [ "action" .= String "setPointsGate"
    , "amount" .= getStageNeededPoints
    ]
  toJSON (PSQLExists { .. }) = object
    [ "action" .= String "psql_exists_macro"
    , "target" .= getStageTarget
    , "query" .= getStageQuery
    , "positiveActions" .= getStagePositiveActions
    , "negativeActions" .= getStageNegativeActions
    ]
  toJSON (PSQLQuery { .. }) = object
    [ "action" .= String "psql_query_macro"
    , "target" .= getStageTarget
    , "query" .= getStageQuery
    , "recordInto" .= getStageRecordVariable
    ]
  toJSON (PSQLAnswerQuery { .. }) = object
    [ "action" .= String "psql_answer_query_macro"
    , "target" .= getStageTarget
    , "recordInto" .= getStageRecordVariable
    ]
  toJSON (PSQLGenerateDatabase { .. }) = object
    [ "action" .= String "psql_generate_database"
    , "target" .= getStageTarget
    , "database" .= getStageDatabaseInfo
    ]
  toJSON (CopyAnswer { .. }) = object
    [ "action" .= String "copyAnswer"
    , "target" .= getStageTarget
    , "filePath" .= getStageFilePath
    ]
  toJSON (PSQLColumnTypeCheck { .. }) = object
    [ "action" .= String "psql_column_type_check"
    , "target" .= getStageTarget
    , "schema" .= getStageDatabaseSchema
    , "tableName" .= getStageTableName
    , "columnName" .= getStageColumnName
    , "awaitedType" .= getStageAwaitedType
    , "positiveActions" .= getStagePositiveActions
    , "negativeActions" .= getStageNegativeActions
    ]
  toJSON (PSQLTableExists { .. }) = object
    [ "action" .= String "psql_table_exists"
    , "target" .= getStageTarget
    , "schema" .= getStageDatabaseSchema
    , "tableName" .= getStageTableName
    , "positiveActions" .= getStagePositiveActions
    , "negativeActions" .= getStageNegativeActions
    ]

instance FromJSON StandCheckStage where
  parseJSON = withObject "StandCheckStage" $ \v -> case K.lookup "action" v of
    Nothing -> fail "No action specified"
    (Just (String "copy")) -> CopyFile
      <$> v .: "target"
      <*> v .: "content"
      <*> v .: "path"
    (Just (String "command")) -> ExecuteCommand
      <$> v .: "target"
      <*> v .: "command"
      <*> v .: "formatOutput"
      <*> v .: "recordInto"
      <*> v .: "reportError"
    (Just (String "points")) -> AddPoints
      <$> v .: "amount"
    (Just (String "compareVars")) -> CompareVariables
      <$> v .: "first"
      <*> v .: "second"
      <*> v .: "positiveActions"
      <*> v .: "negativeActions"
    (Just (String "compareStatusCode")) -> CompareLatestStatusCode
      <$> v .: "awaitedStatus"
      <*> v .: "positiveActions"
      <*> v .: "negativeActions"
    (Just (String "declare")) -> DeclareVariable
      <$> v .: "variableName"
      <*> v .: "variableValue"
    (Just (String "stopCheck")) -> pure StopCheck
    (Just (String "acceptCheck")) -> pure AcceptCheck
    (Just (String "displayMessage")) -> DisplayMessage
      <$> v .: "message"
      <*> v .: "title"
    (Just (String "displayVariable")) -> DisplayVariable
      <$> v .: "variableName"
      <*> v .: "message"
      <*> v .: "title"
    (Just (String "setPointsGate")) -> SetPointsGate
      <$> v .: "amount"
    (Just (String "copyAnswer")) -> CopyAnswer
      <$> v .: "target"
      <*> v .: "filePath"
    (Just (String "psql_exists_macro")) -> PSQLExists
      <$> v .: "target"
      <*> v .: "query"
      <*> v .: "positiveActions"
      <*> v .: "negativeActions"
    (Just (String "psql_query_macro")) -> PSQLQuery
      <$> v .: "target"
      <*> v .: "query"
      <*> v .:? "recordInto"
    (Just (String "psql_answer_query_macro")) -> PSQLAnswerQuery
      <$> v .: "target"
      <*> v .:? "recordInto"
    (Just (String "psql_generate_database")) -> PSQLGenerateDatabase
      <$> v .: "target"
      <*> v .: "database"
    (Just (String "psql_table_exists")) -> PSQLTableExists
      <$> v .: "target"
      <*> v .: "schema"
      <*> v .: "tableName"
      <*> v .: "positiveActions"
      <*> v .: "negativeActions"
    (Just (String "psql_column_type_check")) -> PSQLColumnTypeCheck
      <$> v .: "target"
      <*> v .: "schema"
      <*> v .: "tableName"
      <*> v .: "columnName"
      <*> v .: "awaitedType"
      <*> v .: "positiveActions"
      <*> v .: "negativeActions"
    _anyOther -> fail "Wrong action type, excepted string!"


-- развертка макросов в простые составные блоки
convertStandCheckList :: EndpointsConfiguration -> T.Text -> [StandCheckStage] -> IO (Either String [StandCheckStage])
convertStandCheckList endpoints answer stages = let
  randomFilePath :: Int -> String -> (String, String)
  randomFilePath ln ext = ("/" <> r <> "." <> ext, r) where
    r = unsafeRandomString ln
  f :: StandCheckStage -> IO (Either String [StandCheckStage])
  f (PSQLGenerateDatabase target db) = do
    resp <- convertCreateDB' endpoints db
    (path, _) <- return $ randomFilePath 15 "sql"
    case resp of
      (DBApiResult query) ->
        return $
          return
            [ DeclareVariable "GENERATED_DATABASE" query,
              CopyFile { getStageTarget = target, getStageFilePath = path, getStageFileContent = query },
              ExecuteCommand { getStageTarget = target, getStageCommand = "psql -f " <> T.pack path, getStageFormatOutput = True, getStageRecordVariable = Just "GENERATED_DATABASE_RESULT", getStageReportError = False }
            ]
      (DBApiError err) -> return $ Left ("Ошибка проверки БД: " <> T.unpack err)
      _anyOther -> return $ Left "Неизвестная ошибка со стороны проверки БД"
  f (CopyAnswer { .. }) = return $
    return
      [ CopyFile { getStageTarget = getStageTarget, getStageFilePath = getStageFilePath, getStageFileContent = answer }
      ]
  f (PSQLTableExists { .. }) = do
    (path, name) <- return $ randomFilePath 15 "sql"
    let query = "SELECT CASE WHEN EXISTS(SELECT * FROM pg_tables WHERE tablename = '" <> getStageTableName <> "' AND schemaname = '" <> getStageDatabaseSchema <> "') THEN 1 ELSE 0 END;"
    return $
      return
        [ CopyFile
          { getStageTarget = getStageTarget
          , getStageFilePath = path
          , getStageFileContent = query
          }
        , DeclareVariable (T.pack path) query
        , ExecuteCommand
          { getStageTarget = getStageTarget
          , getStageCommand = T.pack $ "psql -f " <> path <> " --csv -t"
          , getStageFormatOutput = True
          , getStageRecordVariable = (Just . T.pack) name
          , getStageReportError = False
          }
        , DeclareVariable (T.pack $ path <> "-correct") "1"
        , CompareVariables
          { getStageFirstV = T.pack $ path <> "-correct"
          , getStageSecondV = T.pack name
          , getStagePositiveActions = getStagePositiveActions
          , getStageNegativeActions = getStageNegativeActions
          }
        ]
  f (PSQLColumnTypeCheck { .. }) = do
    (path, name) <- return $ randomFilePath 15 "sql"
    let query = "SELECT CASE WHEN ((SELECT data_type FROM information_schema.columns WHERE table_name = '" <> getStageTableName <> "' AND column_name = '" <> getStageColumnName <> "' AND table_schema = '" <> getStageDatabaseSchema <> "') = '" <> getStageAwaitedType <> "') THEN 1 ELSE 0 END;"
    return $
      return
        [ CopyFile { getStageTarget = getStageTarget, getStageFilePath = path, getStageFileContent = query }
        , DeclareVariable (T.pack path) query
        , ExecuteCommand
          { getStageTarget = getStageTarget
          , getStageCommand = T.pack $ "psql -f " <> path <> " --csv -t"
          , getStageFormatOutput = True
          , getStageRecordVariable = (Just . T.pack) name
          , getStageReportError = False
          }
        , DeclareVariable (T.pack $ path <> "-correct") "1"
        , CompareVariables
          { getStageFirstV = T.pack $ path <> "-correct"
          , getStageSecondV = T.pack name
          , getStagePositiveActions = getStagePositiveActions
          , getStageNegativeActions = getStageNegativeActions
          }
        ]
  f (PSQLExists { .. }) = do
    (path, name) <- return $ randomFilePath 15 "sql"
    let query' = if T.last getStageQuery == ';' then T.init getStageQuery else getStageQuery
    let query = "select (CASE WHEN EXISTS(" <> query' <> ") THEN 1 ELSE 0 END);"
    return $
      return
        [ CopyFile { getStageTarget = getStageTarget, getStageFilePath = path, getStageFileContent = query }
        , DeclareVariable (T.pack path) query
        , ExecuteCommand
          { getStageTarget = getStageTarget
          , getStageCommand = T.pack $ "psql -f " <> path <> " --csv -t"
          , getStageFormatOutput = True
          , getStageRecordVariable = (Just . T.pack) name
          , getStageReportError = False
          }
        , DeclareVariable (T.pack $ path <> "-correct") "1"
        , CompareVariables
          { getStageFirstV = T.pack $ path <> "-correct"
          , getStageSecondV = T.pack name
          , getStagePositiveActions = getStagePositiveActions
          , getStageNegativeActions = getStageNegativeActions
          }
        ]
  f (PSQLQuery { .. }) = do
    (path, _) <- return $ randomFilePath 15 "sql"
    return $
      return
        [ CopyFile { getStageTarget = getStageTarget, getStageFilePath = path, getStageFileContent = getStageQuery }
        , DeclareVariable (T.pack path) getStageQuery
        , ExecuteCommand
          { getStageTarget = getStageTarget
          , getStageCommand = T.pack $ "psql -f " <> path <> " --csv"
          , getStageFormatOutput = True
          , getStageRecordVariable = getStageRecordVariable
          , getStageReportError = False
          }
        ]
  f (PSQLAnswerQuery { .. }) = do
    (path, _) <- return $ randomFilePath 15 "sql"
    return $
      return
        [ CopyFile { getStageTarget = getStageTarget, getStageFilePath = path, getStageFileContent = answer }
        , DeclareVariable (T.pack path) answer
        , ExecuteCommand
          { getStageTarget = getStageTarget
          , getStageCommand = T.pack $ "psql -f " <> path <> " --csv"
          , getStageFormatOutput = True
          , getStageRecordVariable = getStageRecordVariable
          , getStageReportError = True
          }
        ]
  f other = return $ return [other]
  in do
    res <- mapM f stages
    if any isLeft res
       then return (head $ filter isLeft res)
       else do
        (return . Right) $ concatMap (fromRight []) res
