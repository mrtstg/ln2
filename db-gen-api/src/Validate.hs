{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Validate (validateDatabase, validateTable) where

import           Control.Monad              (when)
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Types.Column
import           Types.Database
import           Types.Table
import           Types.ValidationError

validateTable :: TableData -> ValidationMonad ()
validateTable (TableData { getTableColumns = columns, getTableName = tName }) = let
  checkTableDuplicates :: [Text] -> [ColumnData] -> ValidationMonad ()
  checkTableDuplicates _ [] = return ()
  checkTableDuplicates acc ((ColumnData { getColumnName = colName }):cls) = if colName `elem` acc then
    throwError (DuplicateColumn tName colName)
    else checkTableDuplicates (colName:acc) cls
  isTableEmpty :: ValidationMonad ()
  isTableEmpty = when (null columns) $ throwError (EmptyTable tName)
  in do
    if (length . filter getColumnPrimary) columns > 1 then throwError (TooManyPrimaryKeys tName) else do
      () <- isTableEmpty
      () <- checkTableDuplicates [] columns
      return ()

validateDatabase :: DatabaseData -> ValidationMonad ()
validateDatabase (DatabaseData tables) = let
  checkTableDuplicates :: [Text] -> [TableData] -> ValidationMonad ()
  checkTableDuplicates _ [] = return ()
  checkTableDuplicates acc ((TableData { getTableName = tName }):cls) = if tName `elem` acc then
    throwError (DuplicateTable tName)
    else checkTableDuplicates (tName:acc) cls
  in do
  () <- checkTableDuplicates [] tables
  _ <- mapM validateTable tables
  return ()
