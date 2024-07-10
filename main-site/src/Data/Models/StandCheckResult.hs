{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.StandCheckResult
  ( StandCheckResult(..)
  , defaultCheckResult
  , StandCheckResultWrapper(..)
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap        as K
import           Data.Models.CheckMessage (CheckMessage)

data StandCheckResultWrapper = StandCheckResultWrapper
  { getWrapperResult :: !(Maybe StandCheckResult)
  , getWrapperStatus :: !String
  } deriving Show

instance FromJSON StandCheckResultWrapper where
  parseJSON = withObject "StandCheckResultWrapper" $ \v -> StandCheckResultWrapper
    <$> v .: "result"
    <*> v .: "status"

data StandCheckResult = StandCheckResult
  { getCheckScore     :: !Int
  , getCheckScoreGate :: !Int
  , getCheckAccepted  :: !Bool
  , getCheckValues    :: !(K.KeyMap String)
  , getCheckMessages  :: ![CheckMessage]
  } deriving Show

instance ToJSON StandCheckResult where
  toJSON (StandCheckResult {..}) = object
    [ "score" .= getCheckScore
    , "scoreGate" .= getCheckScoreGate
    , "accepted" .= getCheckAccepted
    , "values" .= getCheckValues
    , "messages" .= getCheckMessages
    ]

instance FromJSON StandCheckResult where
  parseJSON = withObject "StandCheckResult" $ \v -> StandCheckResult
    <$> v .: "score"
    <*> v .: "scoreGate"
    <*> v .: "accepted"
    <*> v .: "values"
    <*> v .: "messages"

defaultCheckResult :: StandCheckResult
defaultCheckResult = StandCheckResult 0 0 False K.empty []
