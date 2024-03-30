{-# LANGUAGE OverloadedStrings #-}
module Data.Models.StandCheckResult (StandCheckResult(..), defaultCheckResult) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap as K

data StandCheckResult = StandCheckResult
  { getCheckScore     :: !Int
  , getMaxCheckScore  :: !Int
  , getRecordedValues :: !(K.KeyMap Value)
  } deriving (Show, Eq)

instance ToJSON StandCheckResult where
  toJSON (StandCheckResult score maxScore vals) = object
    [ "score" .= score
    , "maxScore" .= maxScore
    , "values" .= vals
    ]

instance FromJSON StandCheckResult where
  parseJSON = withObject "StandCheckResult" $ \v -> StandCheckResult
    <$> v .: "score"
    <*> v .: "maxScore"
    <*> v .: "values"

defaultCheckResult :: StandCheckResult
defaultCheckResult = StandCheckResult 0 0 K.empty
