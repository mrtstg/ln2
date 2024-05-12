{-# LANGUAGE RecordWildCards #-}
module Render (renderColumn, renderTableCreate, renderDatabaseCreate) where

import           Data.List      (intercalate)
import           Data.Text      (unpack)
import           Types.Column
import           Types.Database
import           Types.Table

renderColumn :: ColumnData -> String
renderColumn (ColumnData { .. }) =
  ['"']
  <> unpack getColumnName
  <> ['"', ' ']
  <> show getColumnType
  <> " "
  <> (if getColumnNullable then "NULL " else "")
  <> (if getColumnUnique then "UNIQUE " else "")
  <> (if getColumnPrimary then "PRIMARY KEY " else "")

renderTableCreate :: TableData -> String
renderTableCreate (TableData { .. }) = "CREATE TABLE " <> ['"'] <> unpack getTableName <> ['"'] <> "(" <> intercalate "," (map renderColumn getTableColumns) <> ");"

renderDatabaseCreate :: DatabaseData -> String
renderDatabaseCreate (DatabaseData tables) = intercalate "\n" (map renderTableCreate tables)
