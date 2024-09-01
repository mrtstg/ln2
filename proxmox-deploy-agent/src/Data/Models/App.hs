module Data.Models.App
  ( App(..)
  ) where

import           Data.Models.Endpoints
import           Data.Models.Proxmox.Configuration
import           Network.AMQP                      (Connection)

data App = App
  { endpoints            :: !EndpointsConfiguration
  , rabbitConnection     :: !Connection
  , proxmoxConfiguration :: !ProxmoxConfiguration
  , devEnabled           :: !Bool
  }
