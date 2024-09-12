{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Proxmox.Template
  ( MachineTemplate'(..)
  , MachineTemplatePatch(..)
  , MachineTemplateCreate(..)
  ) where

import           Data.Aeson
import           Data.Text  (Text)

data MachineTemplate' = MachineTemplate'
  { getMachineTemplateId      :: !Int
  , getMachineTemplateName    :: !Text
  , getMachineTemplateComment :: !Text
  } deriving Show

instance ToJSON MachineTemplate' where
  toJSON (MachineTemplate' { .. }) = object
    [ "id" .= getMachineTemplateId
    , "name" .= getMachineTemplateName
    , "comment" .= getMachineTemplateComment
    ]

data MachineTemplateCreate = MachineTemplateCreate
  { getTemplateCreateId      :: !Int
  , getTemplateCreateName    :: !Text
  , getTemplateCreateComment :: !Text
  } deriving Show

instance FromJSON MachineTemplateCreate where
  parseJSON = withObject "MachineTemplateCreate" $ \v -> MachineTemplateCreate
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "comment"

data MachineTemplatePatch = MachineTemplatePatch
  { getTemplatePatchId      :: !(Maybe Int)
  , getTemplatePatchName    :: !Text
  , getTemplatePatchComment :: !(Maybe Text)
  } deriving Show

instance FromJSON MachineTemplatePatch where
  parseJSON = withObject "MachineTemplatePatch" $ \v -> MachineTemplatePatch
    <$> v .:? "id"
    <*> v .:? "name" .!= ""
    <*> v .:? "comment"