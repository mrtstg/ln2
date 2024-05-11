{-# LANGUAGE OverloadedStrings #-}
module Types.Column (ColumnType(..), ColumnData(..)) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap as K
import           Data.Text         (Text)
import           Numeric.Natural

data ColumnType = CharVariable !(Maybe Natural) | Char !(Maybe Natural) | Serial | Boolean | Integer | Text deriving (Eq)

instance FromJSON ColumnType where
  parseJSON = withObject "ColumnType" $ \v -> case K.lookup "type" v of
    Nothing                   -> fail "No column type specified!"
    (Just (String "varchar")) -> CharVariable <$> v .:? "size"
    (Just (String "char"))    -> Char <$> v .:? "size"
    (Just (String "serial"))  -> pure Serial
    (Just (String "boolean")) -> pure Boolean
    (Just (String "integer")) -> pure Integer
    (Just (String "text"))    -> pure Text
    _wrongType                -> fail "Wrong column type!"

instance Show ColumnType where
  show (CharVariable Nothing)     = "VARCHAR"
  show (CharVariable (Just size)) = "VARCHAR(" <> show size <> ")"
  show (Char Nothing)             = "CHAR"
  show (Char (Just size))         = "CHAR(" <> show size <> ")"
  show Serial                     = "SERIAL"
  show Boolean                    = "BOOL"
  show Integer                    = "INT"
  show Text                       = "TEXT"

type IsPrimary = Bool
type IsNull = Bool
type IsUnique = Bool

data ColumnData = ColumnData
  { getColumnName     :: !Text
  , getColumnType     :: !ColumnType
  , getColumnPrimary  :: !IsPrimary
  , getColumnNullable :: !IsNull
  , getColumnUnique   :: !IsUnique
  } deriving (Show)

instance FromJSON ColumnData where
  parseJSON = withObject "ColumnData" $ \v -> ColumnData
    <$> v .: "name"
    <*> v .: "type"
    <*> v .:? "primary" .!= False
    <*> v .:? "null" .!= False
    <*> v .:? "unique" .!= False
