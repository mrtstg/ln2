{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Proxmox.Agent
  ( AgentRequest(..)
  ) where

import           Data.Aeson

data AgentRequest = AgentRequest
  { getAgentRequestDisplay :: !Int
  , getAgentRequestVMID    :: !Int
  } deriving Show

-- notice: vmid field is omitted
instance ToJSON AgentRequest where
  toJSON (AgentRequest { .. }) = object
    [ "display" .= getAgentRequestDisplay
    ]
