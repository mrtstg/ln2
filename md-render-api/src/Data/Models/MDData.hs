{-# LANGUAGE OverloadedStrings #-}
module Data.Models.MDData (MarkdownData(..)) where

import           Data.Aeson
import           Data.Text

newtype MarkdownData = MarkdownData Text deriving (Show, Eq)

instance FromJSON MarkdownData where
  parseJSON = withObject "MarkdownData" $ \v -> MarkdownData
    <$> v .: "data"
