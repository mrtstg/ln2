{-# LANGUAGE OverloadedStrings #-}
module Data.Models.Table (TableData(..)) where

import           Data.Aeson
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
