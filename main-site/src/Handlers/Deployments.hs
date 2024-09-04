module Handlers.Deployments (getDeploymentsR) where

import           Data.Models.User (UserDetails (..))
import           Foundation
import           Handlers.Utils
import           Yesod.Core

getDeploymentsR :: Handler Html
getDeploymentsR = do
  (UserDetails { .. }) <- requireAuth
  undefined
