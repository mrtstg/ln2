{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Validate (validateDatabase, validateTable) where

import           Control.Monad.Except
import           Data.Models.Column
import           Data.Models.Database
import           Data.Models.Table
import           Data.Models.ValidationError
import           Data.Text                   (Text)
import qualified Data.Text                   as T

type TableName = Text
type ColumnName = Text

splitColumnReference :: TableName -> ColumnName -> Text -> ValidationMonad (Text, Text)
splitColumnReference tName cName reference = case T.count "." reference of
  1         -> do
    let res = T.splitOn "." reference
    return (head res, res !! 1)
  _anyOther -> throwError (InvalidRefenceFormat tName cName)

validateRefenceCycles :: DatabaseData -> ValidationMonad ()
validateRefenceCycles db = helper [] (getReferences db) where
  helper :: [(TableName, TableName)] -> [(TableName, TableName)] -> ValidationMonad ()
  helper _ [] = return ()
  helper acc ((t, t'):rs) = if (t', t) `elem` acc then throwError (CurricularReference t t') else helper ((t,t'):acc) rs

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
  allowedSyms = ['а'..'я'] ++ ['a'..'z'] ++ ['_']
  isTableNameValid :: ValidationMonad ()
  isTableNameValid = if T.null tName then throwError EmptyTableName else do
    when (any (`notElem` allowedSyms) (T.unpack $ T.toLower tName)) $ throwError (InvalidTableName tName)
  isColsValid :: ValidationMonad ()
  isColsValid = case filter (any (`notElem` allowedSyms) . fst) $ map (\el -> ((T.unpack . T.toLower . getColumnName) el, el)) columns of
    []             -> return ()
    ((_, cName):_) -> throwError $ InvalidColumnName tName (getColumnName cName)
  in do
    if (length . filter getColumnPrimary) columns > 1 then throwError (TooManyPrimaryKeys tName) else do
      () <- when (any (T.null . T.strip . getColumnName) columns) $ throwError (EmptyColumnAt tName)
      () <- isTableEmpty
      () <- isColsValid
      () <- isTableNameValid
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
  () <- validateRefenceCycles db
  return ()
