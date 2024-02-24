{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.Task (postTaskCreateR) where

import           Data.Aeson
import           Data.Models.QueueTask  (QueueTask (QueueTask))
import           Data.Models.Stand
import           Data.Models.StandCheck
import qualified Data.Text              as T
import           Data.UUID.V4           (nextRandom)
import           Data.Yaml              (ParseException, decodeFileEither)
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
          runDB $ do
            _ <- insertKey (TaskKey taskUuid) (Task standName "queued" Nothing)
            return ()
          liftIO $ putQueueTask rabbitConnection $ QueueTask taskUuid standData standActions
          sendStatusJSON status200 $ object [ "uuid" .= taskUuid ]
