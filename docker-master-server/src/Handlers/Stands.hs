{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Stands (getStandsR, postStandsCreateR, getStandsCreateR, getStandContainersR) where

import           Data.Aeson
import           Data.Models.Stand
import qualified Data.Text                 as T
import qualified Data.Yaml                 as Y
import           Foundation
import           Network.HTTP.Types.Status
import           System.FilePath           (dropExtension)
import           Utils
import           Yesod.Core

validateAndParseStand :: String -> Handler StandData
validateAndParseStand standName = do
  App { .. } <- getYesod
  findRes <- liftIO $ findYMLByName standsFolder (standName)
  case findRes of
    Nothing -> sendStatusJSON notFound404 $ object ["error" .= String "Not found"]
    (Just p') -> do
      parseRes <- liftIO $ Y.decodeFileEither p' :: Handler (Either Y.ParseException StandData)
      case parseRes of
        (Left _) -> sendStatusJSON status505 $ object ["error" .= String "Internal error"]
        (Right r) -> return r

getStandsR :: Handler Value
getStandsR = do
  App { .. } <- getYesod
  files <- liftIO $ listYMLFiles standsFolder
  sendStatusJSON status200 $ map dropExtension files

getStandContainersR :: T.Text -> Handler Value
getStandContainersR standName = do
  standData <- validateAndParseStand (T.unpack standName)
  sendStatusJSON status200 $ getContainersSummary standData

getStandsCreateR :: T.Text -> Handler Value
getStandsCreateR standName = do
  standData <- validateAndParseStand (T.unpack standName)
  sendStatusJSON status200 $ standData

postStandsCreateR :: T.Text -> Handler Value
postStandsCreateR standName = do
  App { .. } <- getYesod
  standInfo <- requireCheckJsonBody :: Handler StandData
  case validateStandCheck standInfo [] (getStandDefaultActions standInfo) of
    (Left e) -> sendStatusJSON badRequest400 $ object ["error" .= (String . T.pack) e]
    (Right ()) -> do
      findRes <- liftIO $ findYMLByName' standsFolder (T.unpack standName)
      case findRes of
        (Just _) -> sendStatusJSON badRequest400 $ object ["error" .= String "Already exists"]
        Nothing -> do
          _ <- liftIO $ Y.encodeFile (createYMLPath standsFolder (T.unpack standName)) standInfo
          sendResponseStatus status204 ()
