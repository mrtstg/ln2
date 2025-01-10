module Foundation.Class
  ( EndpointsApp(..)
  , AuthBypassApp(..)
  , StartDisplayApp(..)
  , StartVMIDApp(..)
  ) where

import           Data.Models.Endpoints

class EndpointsApp a where
  appEndpoints :: a -> EndpointsConfiguration

class AuthBypassApp a where
  appAuthBypass :: a -> Bool

-- used in proxmox
class StartDisplayApp a where
  startDisplay :: a -> Int

class StartVMIDApp a where
  startVMID :: a -> Int
