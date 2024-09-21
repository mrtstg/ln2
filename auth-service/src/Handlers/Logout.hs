{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Logout (postLogoutRouteR) where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Foundation
import           Network.HTTP.Types
import           Redis.Common
import           Yesod.Core

newtype TokenRequest = TokenRequest { getTokenRequest :: String } deriving (Show)

instance FromJSON TokenRequest where
  parseJSON = withObject "TokenRequest" $ \v -> TokenRequest <$>
    v .: "token"

postLogoutRouteR :: Handler Value
postLogoutRouteR = do
  App { .. } <- getYesod
  TokenRequest { .. } <- requireCheckJsonBody
  userLoginBS' <- liftIO $ getValue' redisPool getTokenRequest
  case userLoginBS' of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Session not found!" ]
    (Just userLoginBS) -> do
      liftIO $ deleteValue' redisPool getTokenRequest
      liftIO $ deleteValue' redisPool ("token-" <> BS.unpack userLoginBS)
      sendResponseStatus status204 ()
