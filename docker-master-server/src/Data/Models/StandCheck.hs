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
  { getStageVariableName  :: !T.Text
  , getStageVariableValue :: !Value
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
    _anyOther -> fail "Wrong action type, excepted string!"
