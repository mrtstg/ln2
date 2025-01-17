{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rabbit
  ( prepareRabbitConsumer
  , rabbitResultConsumer
  ) where

import           Control.Monad                     (unless, when)
import           Data.Aeson
import qualified Data.ByteString                   as BS
import           Data.Models.QueueTaskResponse
import           Data.Models.Rabbit.ConnectionData
import           Data.Models.StandCheckResult
import           Database.Persist
import           Database.Persist.Postgresql
import           Foundation
import           Network.AMQP
import           System.Environment                (lookupEnv)
import           Text.Read                         (readMaybe)
import           Utils.Environment

prepareRabbitConsumer :: Connection -> ((Message, Envelope) -> IO ()) -> IO ConsumerTag
prepareRabbitConsumer rCon cCallback = do
  chan <- openChannel rCon
  _ <- declareQueue chan newQueue { queueName = "mainSiteQueue" }
  consumeMsgs chan "mainSiteQueue" Ack cCallback

rabbitResultConsumer :: App -> (Message, Envelope) -> IO ()
rabbitResultConsumer App { .. } (msg, env) = do
  let msgBody' = msgBody msg
  let msgParseRes = eitherDecode msgBody' :: Either String QueueTaskResponse
  case msgParseRes of
    (Left e) -> do
      print e
      ackEnv env
    (Right (QueueTaskResponse { .. })) -> do
      let solveUUID = getTaskResponseUUID
      case getTaskResult of
        Nothing -> ackEnv env
        (Just taskRes@(StandCheckResult { .. })) -> do
          courseSolve' <- flip runSqlPool postgresqlPool $ selectFirst [ CourseSolvesId ==. CourseSolvesKey solveUUID ] []
          case courseSolve' of
            Nothing -> do
              putStrLn "Solve not found!"
              ackEnv env
            (Just (Entity solveId (CourseSolves { .. }))) -> do
              courseTask' <- flip runSqlPool postgresqlPool $ selectFirst [ CourseTaskId ==. courseSolvesTaskId ] []
              case courseTask' of
                Nothing -> do
                  putStrLn "Course task not found!"
                  ackEnv env
                (Just (Entity taskId (CourseTask { .. }))) -> do
                  flip runSqlPool postgresqlPool $ do
                    update solveId [ CourseSolvesCorrect =. getCheckAccepted ]
                    when getCheckAccepted $ do
                      taskAccepted <- exists
                        [ CourseSolveAcceptionTaskId ==. taskId
                        , CourseSolveAcceptionUserId ==. courseSolvesUserId]
                      unless taskAccepted $ do
                        _ <- insert (CourseSolveAcception courseSolvesUserId taskId)
                        return ()
                  ackEnv env
