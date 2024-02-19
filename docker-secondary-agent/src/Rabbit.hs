{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Rabbit
  ( prepareRabbitQuery
  , RabbitConnectionData(..)
  , getEnvRabbitConnectionData
  , prepareRabbitConsumer
  , rabbitResultConsumer
  , putQueueRequest
  , putQueueRequest'
  , putQueueTaskResponse
  ) where

import           Control.Concurrent            (threadDelay)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.Models.App               (App (..))
import           Data.Models.QueueTask
import           Data.Models.QueueTaskResponse
import           Network.AMQP
import           System.Environment            (lookupEnv)
import           Text.Read                     (readMaybe)

data RabbitConnectionData = RConData
  { getRConUser :: !String
  , getRConPass :: !String
  , getRConHost :: !String
  , getRConPort :: !Int
  } deriving (Show, Eq)

putQueueTaskResponse :: Connection -> QueueTaskResponse -> IO ()
putQueueTaskResponse rCon = putQueueRequest' rCon . encode

putQueueRequest' :: Connection -> BL.ByteString -> IO ()
putQueueRequest' rCon messageBody = do
  chan <- openChannel rCon
  _ <- publishMsg chan "resultsExchange" "" $
    newMsg { msgBody = messageBody, msgDeliveryMode = Just Persistent }
  return ()

putQueueRequest :: Connection -> String -> IO ()
putQueueRequest rCon messageBody = putQueueRequest' rCon (BL.pack messageBody)

rabbitResultConsumer :: App -> (Message, Envelope) -> IO ()
rabbitResultConsumer App { .. } (msg, env) = do
  putStrLn "Got request!"
  let msgBody' = msgBody msg
  let msgParseRes = eitherDecode msgBody' :: Either String QueueTask
  case msgParseRes of
    (Left e) -> do
      putStrLn e
      ackEnv env
    (Right queueTask) -> do
      putQueueTaskResponse rabbitConnection $ QueueTaskResponse (getTaskUUID queueTask) "taken" Nothing
      ackEnv env
      threadDelay 3000000
      putQueueTaskResponse rabbitConnection $ QueueTaskResponse (getTaskUUID queueTask) "finished" Nothing

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
  _ <- declareQueue chan newQueue { queueName = "resultsQueue" }
  declareExchange chan newExchange { exchangeName = "resultsExchange", exchangeType = "direct" }
  bindQueue chan "resultsQueue" "resultsExchange" ""
  return ()

prepareRabbitConsumer :: Connection -> ((Message, Envelope) -> IO ()) -> IO ConsumerTag
prepareRabbitConsumer rCon cCallback = do
  chan <- openChannel rCon
  _ <- declareQueue chan newQueue { queueName = "requestsQueue" }
  consumeMsgs chan "requestsQueue" Ack cCallback
