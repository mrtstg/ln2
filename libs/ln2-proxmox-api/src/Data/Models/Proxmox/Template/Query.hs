{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Proxmox.Template.Query (TemplateQuery(..)) where

import           Data.Aeson
import           Data.Text  (Text)

data TemplateQuery = TemplateQuery
  { getTemplateQueryPageSize :: !Int
  , getTemplateQuery         :: !Text
  , getTemplateQueryPage     :: !Int
  } deriving Show

instance ToJSON TemplateQuery where
  toJSON (TemplateQuery { .. }) = object
    [ "pageSize" .= getTemplateQueryPageSize
    , "query" .= getTemplateQuery
    , "page" .= getTemplateQueryPage
    ]

instance FromJSON TemplateQuery where
  parseJSON = withObject "TemplateQuery" $ \v -> TemplateQuery
    <$> v .: "pageSize"
    <*> v .: "query"
    <*> v .: "page"
