{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Validate (validateDatabase, validateTable) where

import           Control.Monad.Except
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Types.Column
import           Types.Database
import           Types.Table
import           Types.ValidationError

type TableName = Text
type ColumnName = Text

splitColumnReference :: TableName -> ColumnName -> Text -> ValidationMonad (Text, Text)
splitColumnReference tName cName reference = case T.count "." reference of
  1         -> do
    let res = T.splitOn "." reference
    return (head res, res !! 1)
  _anyOther -> throwError (InvalidRefenceFormat tName cName)

validateColumnReference :: DatabaseData -> TableData -> ColumnData -> ValidationMonad ()
validateColumnReference db (TableData tName _) (ColumnData { getColumnName = colName, getColumnReferenceOn = colRef, getColumnType = colType }) = do
  case colRef of
    Nothing -> return ()
    (Just r) -> do
      (tName', cName') <- splitColumnReference tName colName r
      if tName' == tName then throwError (ReferenceLoop tName colName) else do
        case lookupField tName' cName' db of
          Nothing -> throwError (MissingReferenceColumn tName colName tName' cName')
          (Just
            (ColumnData
              { getColumnType = colType'
              , getColumnPrimary = colPrimary
              , getColumnUnique = colUnique }
            )) -> if not (colType `referencableType` colType') then throwError (ReferenceTypeMismatch tName colName tName' cName') else do
              when (not colPrimary && not colUnique) $ throwError (ReferenceConstraintsFailure tName' cName')

validateTableRefences :: DatabaseData -> TableData -> ValidationMonad ()
validateTableRefences db t@(TableData { getTableColumns = cols }) = mapM_ (validateColumnReference db t) cols

validateDatabaseReferences :: DatabaseData -> ValidationMonad ()
validateDatabaseReferences db@(DatabaseData tables) = mapM_ (validateTableRefences db) tables

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
validateDatabase db@(DatabaseData tables) = let
  checkTableDuplicates :: [Text] -> [TableData] -> ValidationMonad ()
  checkTableDuplicates _ [] = return ()
  checkTableDuplicates acc ((TableData { getTableName = tName }):cls) = if tName `elem` acc then
    throwError (DuplicateTable tName)
    else checkTableDuplicates (tName:acc) cls
  in do
  () <- checkTableDuplicates [] tables
  mapM_ validateTable tables
  () <- validateDatabaseReferences db
  return ()
