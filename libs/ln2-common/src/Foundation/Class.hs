module Foundation.Class
  ( EndpointsApp(..)
  , AuthBypassApp(..)
  ) where

import           Data.Models.Endpoints

class EndpointsApp a where
  appEndpoints :: a -> EndpointsConfiguration

class AuthBypassApp a where
  appAuthBypass :: a -> Bool
