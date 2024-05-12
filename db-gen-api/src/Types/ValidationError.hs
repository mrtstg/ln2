{-# LANGUAGE OverloadedStrings #-}
module Types.ValidationError (ValidationError(..), ValidationMonad) where

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

instance Show ValidationError where
  show (TooManyPrimaryKeys tName) = unpack $ "У таблицы " <> tName <> " больше одного первичного ключа!"
  show (DuplicateTable tName) = unpack $ "Создано несколько таблиц с именем " <> tName <> "!"
  show (DuplicateColumn tName cName) = unpack $ "В таблице " <> tName <> " две колонки с именем " <> cName <> "!"
  show (EmptyTable tName) = unpack $ "У таблицы " <> tName <> " не указано колонок!"
