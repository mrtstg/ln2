module Handlers.Response (sendCurryJSON) where

import           Data.Aeson
import           Network.HTTP.Types.Status
import           Yesod.Core

sendCurryJSON :: (MonadHandler m, ToJSON a) => (Status, a) -> m b
sendCurryJSON = uncurry sendStatusJSON
