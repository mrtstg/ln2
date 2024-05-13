{-# LANGUAGE OverloadedStrings #-}
module Data.Models.Table
  ( TableData(..)
  , getTableRefencesAmount
  ) where

import           Data.Aeson
import           Data.Maybe
import           Data.Models.Column
import           Data.Text          (Text)

data TableData = TableData
  { getTableName    :: !Text
  , getTableColumns :: ![ColumnData]
  } deriving (Show)

instance FromJSON TableData where
  parseJSON = withObject "TableData" $ \v -> TableData
    <$> v .: "name"
    <*> v .: "columns"

instance ToJSON TableData where
  toJSON (TableData name cols) = object ["name" .= name, "columns" .= cols]

getTableRefencesAmount :: TableData -> Int
getTableRefencesAmount (TableData { getTableColumns = cols }) = length $ filter (isJust . getColumnReferenceOn) cols
