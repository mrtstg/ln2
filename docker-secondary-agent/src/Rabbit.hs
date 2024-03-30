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
import qualified Data.Either                   as E
import           Data.Models.App               (App (..))
import           Data.Models.QueueTask
import           Data.Models.QueueTaskResponse
import           Data.Models.Stand
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Deploy.Docker
import           Network.AMQP
import           System.Environment            (lookupEnv)
import           System.Timeout
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
      let taskUUID = getTaskUUID queueTask
      let taskUUID' = T.pack $ getTaskUUID queueTask
      putQueueTaskResponse rabbitConnection $ QueueTaskResponse taskUUID "taken" Nothing
      ackEnv env
      (containerRess, networkId') <- defaultRunDocker $ deployStand taskUUID' taskUUID' (getStandData queueTask)
      case networkId' of
        Nothing -> do
          putQueueTaskResponse rabbitConnection $ QueueTaskResponse taskUUID "error" Nothing
          putStrLn "Failed to create network!"
        (Just networkId) -> do
          putQueueTaskResponse rabbitConnection $ QueueTaskResponse taskUUID "processing" Nothing
          if not $ all E.isRight containerRess then do
            putStrLn "Not all containers deployed:"
            mapM_ print containerRess
            putQueueTaskResponse rabbitConnection $ QueueTaskResponse taskUUID "error" Nothing
          else do
            putStrLn "Everything deployed! Launching check..."
            res' <- timeout 60000000 $ executeStandCheck taskUUID' ((getStandDefaultActions . getStandData) queueTask ++ getStandCheck queueTask)
            case res' of
              Nothing -> putQueueTaskResponse rabbitConnection $ QueueTaskResponse taskUUID "timeout" Nothing
              (Just (_, res)) -> do
                putQueueTaskResponse rabbitConnection $ QueueTaskResponse taskUUID "finished" (Just res)
          defaultRunDocker $ destroyStand (E.rights containerRess) networkId

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
