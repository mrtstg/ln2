{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Validate (postValidateTokenR) where

import           Crud                  (getUserAssignedRoles,
                                        getUserDetailsByName)
import           Data.Aeson
import           Data.ByteString.Char8 (unpack)
import           Data.Models.User      (UserDetails (..), userDetailsFromModel)
import qualified Data.Text             as T
import           Database.Persist
import           Foundation
import           Network.HTTP.Types
import           Redis
import           Yesod.Core
import           Yesod.Persist

newtype TokenRequest = TokenRequest { getTokenRequest :: String } deriving (Show)

instance FromJSON TokenRequest where
  parseJSON = withObject "TokenRequest" $ \v -> TokenRequest <$>
    v .: "token"

getUserDetailsWrapper :: T.Text -> Handler (Maybe UserDetails)
getUserDetailsWrapper userName = runDB $ getUserDetailsByName userName

postValidateTokenR :: Handler Value
postValidateTokenR = do
  TokenRequest { getTokenRequest = token } <- requireCheckJsonBody
  App { .. } <- getYesod
  userName' <- liftIO $ getValue' redisPool token
  case userName' of
    Nothing -> sendStatusJSON status403 $ object [ "error" .= String "Unauthorized!" ]
    (Just v') -> do
      let userName = T.pack $ unpack v'
      cacheRes <- getOrCacheJsonValue redisPool (Just 5) ("uDetails-" <> T.unpack userName) (getUserDetailsWrapper userName)
      case cacheRes of
        (Left _) -> sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
        (Right v) -> sendStatusJSON status200 $ toJSON v
