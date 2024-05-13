{-# LANGUAGE OverloadedStrings #-}
module Api.Task
  ( createTask
  , TaskResult(..)
  , createTask''
  , createTask'
  ) where

import           Control.Exception          (catch)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString.Lazy       as LBS
import           Data.Models.Endpoints
import           Data.Models.StandCheck
import           Data.Text                  (Text)
import           Network.HTTP.Simple
import           System.Environment
import           Yesod.Core                 (liftIO)

type StandName = Text

newtype TaskResponse = TaskResponse String deriving (Show, Eq)

newtype TaskErr = TaskErr String

instance FromJSON TaskErr where
  parseJSON = withObject "TaskErr" $ \v -> TaskErr <$> v .: "error"

instance FromJSON TaskResponse where
  parseJSON = withObject "TaskResponse" $ \v -> TaskResponse <$> v .: "uuid"

data TaskResult a = TaskResult !a | TaskError !String deriving (Show, Eq)

taskHandler' :: HttpException -> IO (Either HttpException (TaskResult a))
taskHandler' _ = return $ Right (TaskError "Unknown error!")

createTask'' :: EndpointsConfiguration -> Text -> StandName -> [StandCheckStage] -> IO (TaskResult String)
createTask'' endpoints answer standName standActions = do
  convertRes <- convertStandCheckList endpoints answer standActions
  case convertRes of
    (Left e) -> return $ TaskError e
    (Right actions) -> do
      createTask' standName actions

createTask' :: StandName -> [StandCheckStage] -> IO (TaskResult String)
createTask' standName standActions = do
  r <- runExceptT (createTask standName standActions) `catch` taskHandler'
  case r of
    ~(Right v) -> return v

createTask :: StandName -> [StandCheckStage] -> ExceptT HttpException IO (TaskResult String)
createTask standName standActions = do
  httpApiUrl' <- liftIO $ lookupEnv "DOCKER_MASTER_URL"
  case httpApiUrl' of
    Nothing -> do
      liftIO $ putStrLn "No auth api url provided!"
      return $ TaskError "No auth url!"
    Just httpApiUrl -> do
      let payload = object [ "stand_name" .= String standName, "actions" .= standActions ]
      let reqString = "POST " <> httpApiUrl <> "/task"
      request <- parseRequest reqString
      let requestData = setRequestBodyJSON payload request
      response <- httpBS requestData
      let requestBody = getResponseBody response
      case getResponseStatusCode response of
        v | v `Prelude.elem` [400, 404] -> do
          let parseRes = (eitherDecode . LBS.fromStrict) requestBody
          case parseRes of
            (Left e)              -> return $ TaskError e
            (Right (TaskErr err)) -> return $ TaskError err
        200 -> do
          let parseRes = (eitherDecode . LBS.fromStrict) requestBody
          case parseRes of
            (Left e)                    -> return $ TaskError e
            (Right (TaskResponse uuid)) -> return $ TaskResult uuid
        _unexpectedCode -> return $ TaskError "Unknown error!"
