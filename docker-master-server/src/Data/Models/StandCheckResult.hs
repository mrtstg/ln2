{-# LANGUAGE OverloadedStrings #-}
module Data.Models.StandCheckResult (StandCheckResult(..), defaultCheckResult) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap as K
import           Data.Text         (Text)

data StandCheckResult = StandCheckResult
  { getCheckScore     :: !Int
  , getMaxCheckScore  :: !Int
  , getRecordedValues :: !(K.KeyMap Value)
  , getUserMessages   :: ![Text]
  } deriving (Show, Eq)

instance ToJSON StandCheckResult where
  toJSON (StandCheckResult score maxScore vals messages) = object
    [ "score" .= score
    , "maxScore" .= maxScore
    , "values" .= vals
    , "messages" .= messages
    ]

instance FromJSON StandCheckResult where
  parseJSON = withObject "StandCheckResult" $ \v -> StandCheckResult
    <$> v .: "score"
    <*> v .: "maxScore"
    <*> v .: "values"
    <*> v .: "messages"

defaultCheckResult :: StandCheckResult
defaultCheckResult = StandCheckResult 0 0 K.empty []
