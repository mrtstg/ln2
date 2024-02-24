{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.Task (postTaskCreateR, getTaskR, deleteTaskR) where

import           Data.Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import           Data.Models.QueueTask  (QueueTask (QueueTask))
import           Data.Models.Stand
import           Data.Models.StandCheck
import qualified Data.Text              as T
import           Data.Time              (getCurrentTime)
import           Data.UUID.V4           (nextRandom)
import           Data.Yaml              (ParseException, decodeFileEither)
import           Database.Persist
import           Foundation
import           GHC.Generics
import           Network.HTTP.Types
import           Rabbit                 (putQueueRequest', putQueueTask)
import           Utils
import           Yesod.Core
import           Yesod.Persist

data TaskRequest = TaskReq { getStandName :: !String, getStandActions :: ![StandCheckStage] } deriving (Generic, Show)

instance FromJSON TaskRequest where
  parseJSON = withObject "TaskRequest" $ \v -> TaskReq <$> v .: "stand_name" <*> v .: "actions"

postTaskCreateR :: Handler Value
postTaskCreateR = do
  App { .. } <- getYesod
  TaskReq { getStandName = standName, getStandActions = standActions } <- requireCheckJsonBody :: Handler TaskRequest
  standYml <- liftIO $ findYMLByName standsFolder standName
  case standYml of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String (T.pack $ "Stand " ++ standName ++ " not found!") ]
    (Just standPath) -> do
      parseRes <- liftIO $ decodeFileEither standPath :: Handler (Either ParseException StandData)
      case parseRes of
        (Left e) -> do
          $logError $ (T.pack . show) e
          sendStatusJSON status500 $ object [ "error" .= String "Internal parse error!" ]
        (Right standData) -> do
          taskUuid' <- liftIO nextRandom
          let taskUuid = show taskUuid'
          nowTime <- liftIO getCurrentTime
          runDB $ do
            _ <- insertKey (TaskKey taskUuid) (Task standName "queued" Nothing nowTime)
            return ()
          liftIO $ putQueueTask rabbitConnection $ QueueTask taskUuid standData standActions
          sendStatusJSON status200 $ object [ "uuid" .= taskUuid ]

taskResultToValue :: Maybe BS.ByteString -> Maybe Value
taskResultToValue Nothing   = Nothing
taskResultToValue (Just bs) = decode $ LBS.fromStrict bs

getTaskR :: T.Text -> Handler Value
getTaskR taskUuid = do
  taskObj' <- runDB $ get $ (TaskKey . T.unpack) taskUuid
  case taskObj' of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Task not found!" ]
    (Just (Task { .. })) -> do
      sendStatusJSON status200 $ object [ "result" .= taskResultToValue taskResult ]

deleteTaskR :: T.Text -> Handler Value
deleteTaskR taskUuid = do
  let taskKey = (TaskKey . T.unpack) taskUuid
  taskExists <- runDB $ exists [TaskId ==. taskKey]
  if not taskExists then sendStatusJSON status404 $ object [ "error" .= String "Task not found!" ] else do
    runDB $ delete taskKey
    sendResponseStatus status204 ()
