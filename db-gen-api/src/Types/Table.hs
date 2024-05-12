{-# LANGUAGE OverloadedStrings #-}
module Types.Table (TableData(..)) where

import           Data.Aeson
import           Data.Text    (Text)
import           Types.Column

data TableData = TableData
  { getTableName    :: !Text
  , getTableColumns :: ![ColumnData]
  } deriving (Show)

instance FromJSON TableData where
  parseJSON = withObject "TableData" $ \v -> TableData
    <$> v .: "name"
    <*> v .: "columns"
