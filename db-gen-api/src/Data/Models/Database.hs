{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Database
  ( DatabaseData (..)
  , lookupField
  , getReferences
  ) where

import           Data.Aeson
import           Data.Maybe
import           Data.Models.Column
import           Data.Models.Table
import           Data.Text          (Text)
import qualified Data.Text          as T

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

getReferences :: DatabaseData -> [(TableName, TableName)]
getReferences (DatabaseData tables) = helper [] tables where
  helper :: [(TableName, TableName)] -> [TableData] -> [(TableName, TableName)]
  helper acc []                     = acc
  helper acc (TableData { .. }:tls) = helper (acc ++ map f cols) tls where
    f :: ColumnData -> (TableName, TableName)
    f (ColumnData { .. }) = (getTableName, T.takeWhile (/='.') (fromMaybe "" getColumnReferenceOn))
    cols :: [ColumnData]
    cols = filter (isJust . getColumnReferenceOn) getTableColumns


