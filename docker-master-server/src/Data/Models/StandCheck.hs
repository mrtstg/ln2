{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.StandCheck (StandCheckStage(..)) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap as K
import qualified Data.Text         as T

type StageTarget = String

data StandCheckStage = CopyFile
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
  |
  CompareLatestStatusCode
  { getAwaitedStatus        :: !Int
  , getStagePositiveActions :: ![StandCheckStage]
  , getStageNegativeActions :: ![StandCheckStage]
  }
  | DeclareVariable
  { getStandVariableName  :: !T.Text
  , getStandVariableValue :: !Value
  }
  | DisplayMessage
  { getStandMessage      :: !T.Text
  , getStandMessageTitle :: !T.Text
  }
  | DisplayVariable
  { getStandVariableName :: !T.Text
  , getStandMessage      :: !T.Text
  , getStandMessageTitle :: !T.Text
  }
  | StopCheck {}
  deriving (Show, Eq)

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
    , "variableName" .= getStandVariableName
    , "variableValue" .= getStandVariableValue
    ]
  toJSON StopCheck = object
    [ "action" .= String "stopCheck"
    ]
  toJSON (DisplayMessage { .. }) = object
    [ "action" .= String "displayMessage"
    , "message" .= getStandMessage
    , "title" .= getStandMessageTitle
    ]
  toJSON (DisplayVariable { .. }) = object
    [ "action" .= String "displayVariable"
    , "variableName" .= getStandVariableName
    , "message" .= getStandMessage
    , "title" .= getStandMessageTitle
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
    (Just (String "displayMessage")) -> DisplayMessage
      <$> v .: "message"
      <*> v .: "title"
    (Just (String "displayVariable")) -> DisplayVariable
      <$> v .: "variableName"
      <*> v .: "message"
      <*> v .: "title"
    _anyOther -> fail "Wrong action type, excepted string!"
