{-# LANGUAGE OverloadedStrings #-}
module Types.Database (DatabaseData (..), lookupField) where

import           Data.Aeson
import           Data.Text    (Text)
import           Types.Column
import           Types.Table

newtype DatabaseData = DatabaseData [TableData] deriving Show

instance FromJSON DatabaseData where
  parseJSON = withObject "DatabaseData" $ \v -> DatabaseData
    <$> v .: "tables"

type TableName = Text
type FieldName = Text

lookupField :: TableName -> FieldName -> DatabaseData -> Maybe ColumnData
lookupField tName fName (DatabaseData tables) = do
  case filter ((==tName) . getTableName ) tables of
    [] -> Nothing
    ((TableData _ cols):_) -> case filter ((==fName) . getColumnName) cols of
      []    -> Nothing
      (c:_) -> Just c
