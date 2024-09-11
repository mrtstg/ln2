{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.CourseTaskPayload
  ( CourseTaskPayload(..)
  , CourseTaskType(..)
  , courseTaskTypeFromString
  , suggestPayloadTaskType
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap                  as K
import           Data.Models.Proxmox.Deploy.Network (DeployNetwork)
import           Data.Models.Proxmox.Deploy.VM      (DeployVM)
import           Data.Models.StandCheck
import           Data.Text                          (Text)

data CourseTaskType = ContainerTask | VMTask deriving (Eq, Enum)

instance Show CourseTaskType where
  show ContainerTask = "contaner"
  show VMTask        = "vm"

courseTaskTypeFromString :: String -> Maybe CourseTaskType
courseTaskTypeFromString "container" = Just ContainerTask
courseTaskTypeFromString "vm"        = Just VMTask
courseTaskTypeFromString _           = Nothing

instance FromJSON CourseTaskType where
  parseJSON = withText "CourseTaskType" $ \case
    "container" -> pure ContainerTask
    "vm" -> pure VMTask
    _ -> fail "Unknown type"

instance ToJSON CourseTaskType where
  toJSON VMTask        = String "vm"
  toJSON ContainerTask = String "container"

data CourseTaskPayload = ContainerTaskPayload
  { getPayloadContainerActions :: ![StandCheckStage]
  , getPayloadStandIdentifier  :: !Text
  }
  | VMTaskPayload
  { getPayloadVMs      :: ![DeployVM]
  , getPayloadNetworks :: ![DeployNetwork]
  }

instance ToJSON CourseTaskPayload where
  toJSON (VMTaskPayload { .. }) = object
    [ "type" .= String "vm"
    , "vms" .= getPayloadVMs
    , "networks" .= getPayloadNetworks
    ]
  toJSON (ContainerTaskPayload { .. }) = object
    [ "type" .= String "container"
    , "actions" .= getPayloadContainerActions
    , "standIdentifier" .= getPayloadStandIdentifier
    ]

instance FromJSON CourseTaskPayload where
  parseJSON = withObject "CourseTaskPayload" $ \v -> case K.lookup "type" v of
    Just (String "vm")        -> VMTaskPayload
      <$> v .: "vms"
      <*> v .: "networks"
    Just (String "container") -> ContainerTaskPayload
      <$> v .: "actions"
      <*> v .: "standIdentifier"
    _otherKey                 -> fail "Unknown payload type"

suggestPayloadTaskType :: CourseTaskPayload -> CourseTaskType
suggestPayloadTaskType (ContainerTaskPayload {}) = ContainerTask
suggestPayloadTaskType (VMTaskPayload {})        = VMTask
