{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.ImportUser
  ( getImportUserR
  , postImportUserR
  ) where

import           Api.User
import           Data.ByteString.Lazy  (fromStrict)
import           Data.Csv
import           Data.Models.Endpoints
import           Data.Models.User
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Vector
import           Foundation
import           Handlers.Utils
import           Utils.Auth
import           Yesod.Core
import           Yesod.Form

newtype FileRes = FileRes FileInfo

fileForm :: Html -> MForm Handler (FormResult FileRes, Widget)
fileForm = renderDivs $ FileRes <$> fileAFormReq "Файл"

getImportUserR :: Handler Html
getImportUserR = do
  (UserDetails { .. }) <- requireUserAuth
  let isAdmin = adminRoleGranted getUserRoles
  if not isAdmin then redirect IndexR else do
    (_, enctype) <- generateFormPost fileForm
    defaultLayout $ do
      setTitle "Импорт пользователей"
      [whamlet|
<div .container.p-5>
  <h1 .title> Импорт пользователей
  <a href=/static/users-sample.txt .button download> Скачать образец
  <form .py-3 method=post action=@{ImportUserR} enctype=#{enctype}>
    <label .label>
      Файл для импорта
      <input type=file name=f1 id=hindent2 required accept=".txt,.csv">
    <button .button.is-success> Импортировать
|]

userCreateResToString :: UserCreate -> UserGetResult a -> Text
userCreateResToString (UserCreate { getUserCreateName = name' }) = helper where
  helper :: UserGetResult a -> Text
  helper (UserGetResult _) = "Пользователь " <> name' <> " создан"
  helper (UserGetError e)   = "Ошибка создания (" <> name' <> "): " <> T.pack e
  helper _anyOther = "Ошибка создания (" <> name' <> "): неизвестная ошибка"

postImportUserR :: Handler Html
postImportUserR = let
  f :: EndpointsConfiguration -> (Text, Text, Text) -> Handler Text
  f endpoints (name', login', pass') = do
    let createData = UserCreate login' name' pass'
    r <- liftIO $ createUser' endpoints createData
    return $ userCreateResToString createData r
  in do
  (UserDetails { .. }) <- requireUserAuth
  ((result, _), _) <- runFormPost fileForm
  case result of
    FormSuccess (FileRes fileInfo) -> do
      App { .. } <- getYesod
      fileContent <- fileSourceByteString fileInfo
      let parsedData' = decode HasHeader (fromStrict fileContent) :: (Either String (Vector (Text, Text, Text)))
      res <- case parsedData' of
        (Left _)         -> return []
        (Right userData) -> do
          Prelude.mapM (f endpointsConfiguration) (toList userData)
      defaultLayout $ do
        setTitle "Результаты импорта"
        [whamlet|
<div .container.p-5>
  <a href=@{ImportUserR} .button.is-fullwidth> На страницу импорта
  $case parsedData'
    $of (Left e)
      <article .message.is-danger>
        <div .message-body>
          Ошибка парсинга: #{e}
    $of (Right _)
      $forall msg <- res
        <article .message>
          <div .message-body>
            #{msg}
|]
    _smthWrong -> defaultLayout $ redirect ImportUserR
