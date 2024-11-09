{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Proxmox.Template
  ( MachineTemplate'(..)
  , MachineTemplatePatch(..)
  , MachineTemplateCreate(..)
  , machineTemplateCreateToPatch
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

machineTemplateCreateToPatch :: MachineTemplateCreate -> MachineTemplatePatch
machineTemplateCreateToPatch (MachineTemplateCreate { .. }) = MachineTemplatePatch
  { getTemplatePatchId=Just getTemplateCreateId
  , getTemplatePatchName=Just getTemplateCreateName
  , getTemplatePatchComment=Just getTemplateCreateComment
  }

instance FromJSON MachineTemplateCreate where
  parseJSON = withObject "MachineTemplateCreate" $ \v -> MachineTemplateCreate
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "comment"

data MachineTemplatePatch = MachineTemplatePatch
  { getTemplatePatchId      :: !(Maybe Int)
  , getTemplatePatchName    :: !(Maybe Text)
  , getTemplatePatchComment :: !(Maybe Text)
  } deriving Show

instance FromJSON MachineTemplatePatch where
  parseJSON = withObject "MachineTemplatePatch" $ \v -> MachineTemplatePatch
    <$> v .:? "id"
    <*> v .:? "name"
    <*> v .:? "comment"
