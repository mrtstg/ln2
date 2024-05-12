{-# LANGUAGE OverloadedStrings #-}
module Data.Models.Database (DatabaseData (..), lookupField) where

import           Data.Aeson
import           Data.Models.Column
import           Data.Models.Table
import           Data.Text          (Text)

newtype DatabaseData = DatabaseData [TableData] deriving Show

instance FromJSON DatabaseData where
  parseJSON = withObject "DatabaseData" $ \v -> DatabaseData
    <$> v .: "tables"

instance ToJSON DatabaseData where
  toJSON (DatabaseData tables) = object ["tables" .= tables]

type TableName = Text
type FieldName = Text

lookupField :: TableName -> FieldName -> DatabaseData -> Maybe ColumnData
lookupField tName fName (DatabaseData tables) = do
  case filter ((==tName) . getTableName ) tables of
    [] -> Nothing
    ((TableData _ cols):_) -> case filter ((==fName) . getColumnName) cols of
      []    -> Nothing
      (c:_) -> Just c
