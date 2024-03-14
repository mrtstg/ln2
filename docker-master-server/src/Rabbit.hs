{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Rabbit
  ( prepareRabbitQuery
  , RabbitConnectionData(..)
  , getEnvRabbitConnectionData
  , prepareRabbitConsumer
  , rabbitResultConsumer
  , putQueueRequest
  , putQueueRequest'
  , putQueueTask
  ) where

import           Data.Aeson
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.Models.QueueTask         (QueueTask)
import           Data.Models.QueueTaskResponse
import           Database.Persist
import           Database.Persist.Postgresql
import           Foundation
import           Network.AMQP
import           System.Environment            (lookupEnv)
import           Text.Read                     (readMaybe)

data RabbitConnectionData = RConData
  { getRConUser :: !String
  , getRConPass :: !String
  , getRConHost :: !String
  , getRConPort :: !Int
  } deriving (Show, Eq)

putQueueTask :: Connection -> QueueTask -> IO ()
putQueueTask rCon = putQueueRequest' rCon . encode

putQueueRequest' :: Connection -> BL.ByteString -> IO ()
putQueueRequest' rCon messageBody = do
  chan <- openChannel rCon
  let msg = newMsg { msgBody = messageBody, msgDeliveryMode = Just Persistent }
  _ <- publishMsg chan "requestsExchange" "" msg
  return ()

putQueueRequest :: Connection -> String -> IO ()
putQueueRequest rCon messageBody = putQueueRequest' rCon (BL.pack messageBody)

jsonValueToBytestring :: Maybe Value -> Maybe BS.ByteString
jsonValueToBytestring Nothing  = Nothing
jsonValueToBytestring (Just v) = Just $ (BS.toStrict . encode) v

rabbitResultConsumer :: App -> (Message, Envelope) -> IO ()
rabbitResultConsumer App { .. } (msg, env) = do
  let msgBody' = msgBody msg
  chan <- openChannel rabbitConnection
  _ <- publishMsg chan "mainSiteExchange" "" msg
  --let msgBodyString = (BL.unpack . msgBody) msg
  let msgParseRes = eitherDecode msgBody' :: Either String QueueTaskResponse
  case msgParseRes of
    (Left e) -> do
      putStrLn e
      ackEnv env
    (Right taskResp) -> do
      let taskUUID = getTaskResponseUUID taskResp
      let taskKey = TaskKey taskUUID
      let taskResult = jsonValueToBytestring $ getTaskResult taskResp
      flip runSqlPool postgresqlPool $ do
        update taskKey [ TaskState =. getTaskResponseStatus taskResp, TaskResult =. taskResult ]
      ackEnv env

getEnvRabbitConnectionData :: IO (Maybe RabbitConnectionData)
getEnvRabbitConnectionData = do
  user'' <- lookupEnv "RABBITMQ_DEFAULT_USER"
  pass'' <- lookupEnv "RABBITMQ_DEFAULT_PASS"
  host'' <- lookupEnv "RABBITMQ_HOST"
  port''' <- lookupEnv "RABBITMQ_PORT"
  case port''' of
    Nothing -> return Nothing
    (Just v) -> do
      let port'' = readMaybe v :: Maybe Int
      return $ do
        user' <- user''
        pass' <- pass''
        host' <- host''
        RConData user' pass' host' <$> port''

prepareRabbitQuery :: Connection -> IO ()
prepareRabbitQuery rCon = do
  chan <- openChannel rCon
  _ <- declareQueue chan newQueue { queueName = "mainSiteQueue" }
  declareExchange chan newExchange { exchangeName = "mainSiteExchange", exchangeType = "direct" }
  bindQueue chan "mainSiteQueue" "mainSiteExchange" ""
  _ <- declareQueue chan newQueue { queueName = "requestsQueue" }
  declareExchange chan newExchange { exchangeName = "requestsExchange", exchangeType = "direct" }
  bindQueue chan "requestsQueue" "requestsExchange" ""
  return ()

prepareRabbitConsumer :: Connection -> ((Message, Envelope) -> IO ()) -> IO ConsumerTag
prepareRabbitConsumer rCon cCallback = do
  chan <- openChannel rCon
  _ <- declareQueue chan newQueue { queueName = "resultsQueue" }
  consumeMsgs chan "resultsQueue" Ack cCallback
