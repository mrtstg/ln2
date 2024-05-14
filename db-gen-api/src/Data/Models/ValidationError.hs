{-# LANGUAGE OverloadedStrings #-}
module Data.Models.ValidationError (ValidationError(..), ValidationMonad) where

import           Control.Monad.Trans.Except
import           Data.Text                  (Text, unpack)

type ValidationMonad = Except ValidationError
type TableName = Text
type ColumnName = Text

data ValidationError =
  TooManyPrimaryKeys !TableName
  | DuplicateTable !TableName
  | DuplicateColumn !TableName !ColumnName
  | EmptyTable !TableName
  | ReferenceTypeMismatch !TableName !ColumnName !TableName !ColumnName
  | ReferenceConstraintsFailure !TableName !ColumnName
  | MissingReferenceColumn !TableName !ColumnName !TableName !ColumnName
  | ReferenceLoop !TableName !ColumnName
  | InvalidRefenceFormat !TableName !ColumnName
  | CurricularReference !TableName !TableName
  | EmptyColumnAt !TableName
  | EmptyTableName
  | InvalidTableName !TableName
  | InvalidColumnName !TableName !ColumnName

instance Show ValidationError where
  show (InvalidColumnName tName cName) = unpack $ "Название столбца " <> tName <> "(" <> cName <> ") содержит запрещенные символы!"
  show (InvalidTableName tName) = unpack $ "Название таблицы " <> tName <> " содержит запрещенные символы!"
  show EmptyTableName = "Таблица с пустым именем"
  show (EmptyColumnAt tName) = unpack $ "Колонка с пустым именем в таблице " <> tName
  show (CurricularReference tName tName') = unpack $ "Циклические ссылки между таблицами " <> tName <> " и " <> tName' <> "!"
  show (TooManyPrimaryKeys tName) = unpack $ "У таблицы " <> tName <> " больше одного первичного ключа!"
  show (DuplicateTable tName) = unpack $ "Создано несколько таблиц с именем " <> tName <> "!"
  show (DuplicateColumn tName cName) = unpack $ "В таблице " <> tName <> " две колонки с именем " <> cName <> "!"
  show (EmptyTable tName) = unpack $ "У таблицы " <> tName <> " не указано колонок!"
  show (ReferenceTypeMismatch sourceTable sourceColumn destTable destColumn) = unpack $
    "Поле "
    <> sourceTable
    <> "(" <> sourceColumn <> ") не может ссылаться на поле "
    <> destTable
    <> "(" <> destColumn <> ") из-за несовпадения типов"
  show (ReferenceConstraintsFailure tName cName) = unpack $ "Поле " <> tName <> "(" <> cName <> ") не является уникальным и недоступно для ссылки"
  show (MissingReferenceColumn sourceTable sourceColumn destTable destColumn) = unpack $
    "Поле "
    <> sourceTable
    <> "(" <> sourceColumn <> ") ссылается на несуществующее поле "
    <> destTable
    <> "(" <> destColumn <> ")"
  show (ReferenceLoop tName cName) = unpack $ "Поле " <> tName <> "(" <> cName <> ") ссылается на собственную таблицу"
  show (InvalidRefenceFormat tName cName) = unpack $ "Поле " <> tName <> "(" <> cName <> ") имеет неправильный формат ссылки"
