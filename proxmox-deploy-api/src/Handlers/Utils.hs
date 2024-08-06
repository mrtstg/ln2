module Handlers.Utils (getIntParam) where

import           Data.Text  (Text, unpack)
import           Foundation
import           Text.Read  (readMaybe)
import           Yesod.Core

getIntParam :: Text -> Int -> Handler Int
getIntParam paramName defaultValue = do
  queryValue <- lookupGetParam paramName
  case queryValue of
    Nothing -> return defaultValue
    (Just v) -> do
      let paramV = readMaybe $ unpack v :: Maybe Int
      case paramV of
        Nothing   -> return 1
        (Just v') -> return v'
