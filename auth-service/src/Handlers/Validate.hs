{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Validate
  ( postValidateUserTokenR
  , postValidateServiceTokenR
  ) where

import           Crud                   (getUserAssignedRoles,
                                         getUserDetailsByName)
import           Data.Aeson
import           Data.ByteString.Char8  (unpack)
import qualified Data.Map               as M
import           Data.Models.Auth.Token
import           Data.Models.User       (UserDetails (..), userDetailsFromModel)
import qualified Data.Text              as T
import           Database.Persist
import           Foundation
import           Network.HTTP.Types
import           Redis.Common
import           Web.JWT
import           Yesod.Core
import           Yesod.Persist

newtype TokenRequest t = TokenRequest { getTokenRequest :: t } deriving (Show)

instance (FromJSON t) => FromJSON (TokenRequest t) where
  parseJSON = withObject "TokenRequest" $ \v -> TokenRequest <$>
    v .: "token"

getUserDetailsWrapper :: T.Text -> Handler (Maybe UserDetails)
getUserDetailsWrapper userName = runDB $ getUserDetailsByName userName

postValidateServiceTokenR :: Handler Value
postValidateServiceTokenR = do
  TokenRequest { getTokenRequest = token } <- requireCheckJsonBody
  App { .. } <- getYesod
  let jwtRes = decodeAndVerifySignature (toVerify . hmacSecret $ jwtSecret) token
  case jwtRes of
    Nothing -> sendStatusJSON status400 $ object [ "error" .= String "Invalid signature!" ]
    (Just jwtToken) -> do
      let unregClaims = (unClaimsMap . unregisteredClaims . claims) jwtToken
      case M.lookup "uuid" unregClaims of
        Nothing -> sendStatusJSON status400 $ object [ "error" .= String "Invalid token structure!" ]
        (Just (String uuid)) -> do
          tokenEntity' <- runDB $ selectFirst [ TokenId ==. (TokenKey . T.unpack) uuid ] []
          case tokenEntity' of
            Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Token not found" ]
            (Just (Entity _ Token { .. })) -> do
              sendStatusJSON status200 $ AuthTokenResponse { getTokenResponseService = T.pack tokenService }
        _otherValue -> sendStatusJSON status400 $ object [ "error" .= String "Invalid token structure!" ]

postValidateUserTokenR :: Handler Value
postValidateUserTokenR = do
  TokenRequest { getTokenRequest = token } <- requireCheckJsonBody
  App { .. } <- getYesod
  userName' <- liftIO $ getValue' redisPool token
  case userName' of
    Nothing -> sendStatusJSON status403 $ object [ "error" .= String "Unauthorized!" ]
    (Just v') -> do
      let userName = T.pack $ unpack v'
      cacheRes <- getOrCacheJsonValue redisPool (Just defaultShortCacheTime) ("uDetails-" <> T.unpack userName) (getUserDetailsWrapper userName)
      case cacheRes of
        (Left _) -> sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
        (Right v) -> sendStatusJSON status200 $ toJSON v
