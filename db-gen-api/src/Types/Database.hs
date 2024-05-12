{-# LANGUAGE OverloadedStrings #-}
module Types.Database (DatabaseData (..)) where

import           Data.Aeson
import           Types.Table

newtype DatabaseData = DatabaseData [TableData] deriving Show

instance FromJSON DatabaseData where
  parseJSON = withObject "DatabaseData" $ \v -> DatabaseData
    <$> v .: "tables"
