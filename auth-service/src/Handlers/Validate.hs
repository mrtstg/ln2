{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Validate (postValidateTokenR) where

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

data TokenRequest = TokenRequest { getTokenRequest :: String } deriving (Show)

instance FromJSON TokenRequest where
  parseJSON = withObject "TokenRequest" $ \v -> TokenRequest <$>
    v .: "token"

postValidateTokenR :: Handler Value
postValidateTokenR = do
  TokenRequest { getTokenRequest = token } <- requireCheckJsonBody
  App { .. } <- getYesod
  userName' <- liftIO $ getValue' redisPool token
  case userName' of
    Nothing -> sendStatusJSON status403 $ object [ "error" .= String "Unauthorized!" ]
    (Just v') -> do
      let userName = T.pack $ unpack v'
      userObject' <- runDB $ selectFirst [UserLogin ==. userName] []
      case userObject' of
        Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Not found!" ]
        (Just e) -> do
          sendStatusJSON status200 $ (toJSON  . userDetailsFromModel) e
