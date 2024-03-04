{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handlers.Login (getLoginR, postLoginR) where

import           Data.Aeson
import           Data.Text          (Text, pack, unpack)
import           Foundation
import           Network.HTTP.Types
import           Yesod.Core
import           Yesod.Form

getLoginR :: Handler Html
getLoginR = do
  (widget, enctype) <- generateFormPost loginForm
  defaultLayout $ do
    setTitle "Login"
    [whamlet|
<form method=post action=@{LoginR} enctype=#{enctype}>
  ^{widget}
  <button> Login
|]

data LoginRequest = LoginRequest !Text !Text deriving Show

instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \v -> LoginRequest <$> v .: "login" <*> v .: "password"

loginForm :: Html -> MForm Handler (FormResult LoginRequest, Widget)
loginForm = renderDivs $ LoginRequest
  <$> areq textField "Логин" Nothing
  <*> areq passwordField "Пароль" Nothing

postLoginR :: Handler Value
postLoginR = do
  ((result, _), _) <- runFormPost loginForm
  case result of
    (FormSuccess (LoginRequest login password)) -> sendStatusJSON status200 $ object ["password" .= password, "login" .= login]
    _formError -> sendStatusJSON status400 $ object ["error" .= String "Failed to parse form!"]
