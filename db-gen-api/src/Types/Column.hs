{-# LANGUAGE OverloadedStrings #-}
module Types.Column
  ( ColumnType(..)
  , ColumnData(..)
  , defaultColumn
  , defaultPrimaryKey
  , defaultReference
  , referencableType
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap as K
import           Data.Text         (Text)
import           Numeric.Natural

data ColumnType = CharVariable !(Maybe Natural) | Char !(Maybe Natural) | Serial | Boolean | Integer | Text deriving (Eq)

type ReferenceType = ColumnType
type TargetReferenceType = ColumnType

referencableType :: ReferenceType -> TargetReferenceType -> Bool
referencableType Serial _       = False
referencableType Integer Serial = True
referencableType a b            = a == b

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
  { getColumnName        :: !Text
  , getColumnType        :: !ColumnType
  , getColumnPrimary     :: !IsPrimary
  , getColumnNullable    :: !IsNull
  , getColumnUnique      :: !IsUnique
  , getColumnReferenceOn :: !(Maybe Text)
  } deriving (Show)

instance FromJSON ColumnData where
  parseJSON = withObject "ColumnData" $ \v -> ColumnData
    <$> v .: "name"
    <*> v .: "type"
    <*> v .:? "primary" .!= False
    <*> v .:? "null" .!= False
    <*> v .:? "unique" .!= False
    <*> v .:? "references"

defaultColumn :: Text -> ColumnType -> ColumnData
defaultColumn name t = ColumnData name t False False False Nothing

defaultPrimaryKey :: Text -> ColumnType -> ColumnData
defaultPrimaryKey name t = ColumnData name t True False False Nothing

defaultReference :: Text -> ColumnType -> Text -> ColumnData
defaultReference name t ref = ColumnData name t False False False (Just ref)
