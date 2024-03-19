{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handlers.Login (getLoginR, postLoginR) where

import           Api.Login
import qualified Api.Login                   as L
import           Control.Exception           (catch)
import           Control.Monad.Trans.Except  (runExceptT)
import           Data.Aeson
import qualified Data.ByteString.Char8       as BS
import           Data.Models.UserAuthRequest
import           Data.Text                   (Text, pack, unpack)
import           Foundation
import           Network.HTTP.Simple         (HttpException)
import           Network.HTTP.Types
import           Web.Cookie
import           Yesod.Core
import           Yesod.Form

getLoginR :: Handler Html
getLoginR = do
  (_, enctype) <- generateFormPost loginForm
  defaultLayout $ do
    setTitle "Login"
    [whamlet|
<div .container.pt-5.py-3>
  <h1 .title.is-2> Вход
  <form .pb-2 method=post action=@{LoginR} enctype=#{enctype}>
    <div .required.field>
      <label .label> Логин
      <input .input name=f1 type=text required value="">
    <div .required.field>
      <label .label> Пароль
      <input .input name=f2 type=password required>
    <button type=submit .button.is-fullwidth.is-success> Login
|]

data LoginRequest = LoginRequest !Text !Text deriving Show

instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \v -> LoginRequest <$> v .: "login" <*> v .: "password"

loginForm :: Html -> MForm Handler (FormResult LoginRequest, Widget)
loginForm = renderDivs $ LoginRequest
  <$> areq textField "Логин" Nothing
  <*> areq passwordField "Пароль" Nothing

postLoginR :: Handler Value
postLoginR =
  let
  handler :: HttpException -> IO (Either HttpException (L.AuthResult Text))
  handler _ = return $ Right L.InternalError -- MUST return Right
  in do
  ((result, _), _) <- runFormPost loginForm
  case result of
    (FormSuccess (LoginRequest login password)) -> do
      authRes' <- liftIO $ runExceptT (sendAuthRequest (UserAuthRequest login password)) `catch` handler
      case authRes' of
        Left _ -> error "Unreachable pattern!" -- TODO: REPLACE
        (Right authRes) -> do
          case authRes of
            L.InvalidCredentials -> redirect LoginR
            (AuthResult token) -> do
              setCookie $ defaultSetCookie { setCookieName = BS.pack "session", setCookieValue = BS.pack . unpack $ token }
              redirect ProfileR
            _internalError -> redirect LoginR
    _formError -> do
      r@(UserAuthRequest {}) <- requireCheckJsonBody
      authRes' <- liftIO $ runExceptT (sendAuthRequest r) `catch` handler
      case authRes' of
        Left _ -> error "Unreachable pattern!" -- TODO: REPLACE
        (Right authRes) -> do
          case authRes of
            L.InvalidCredentials -> sendStatusJSON status400 $ object [ "error" .= String "Invalid credentials!" ]
            (AuthResult token) -> do
              sendStatusJSON status200 $ object [ "token" .= String token ]
            _internalError -> sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
