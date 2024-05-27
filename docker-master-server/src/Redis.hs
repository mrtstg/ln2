{-# LANGUAGE OverloadedStrings #-}
module Redis
  ( cacheValue
  , cacheValue'
  , deleteValue
  , getValue
  , getValue'
  , deleteValue'
  , getJsonValue'
  , getOrCacheJsonValue
  , defaultShortCacheTime
  ) where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T
import           Data.Text.Encoding    (encodeUtf8)
import           Database.Redis
import           Foundation
import           Yesod.Core

defaultShortCacheTime :: Integer
defaultShortCacheTime = 5

cacheValue :: Connection -> BS.ByteString -> BS.ByteString -> Maybe Integer -> IO ()
cacheValue conn key value timeout = do
  _ <- runRedis conn $ setOpts key value (SetOpts timeout Nothing Nothing)
  return ()

cacheValue' :: Connection -> String -> String -> Maybe Integer -> IO ()
cacheValue' conn key value = cacheValue conn (BS.pack key) (BS.pack value)

deleteValue :: Connection -> BS.ByteString -> IO ()
deleteValue conn key = do
  _ <- runRedis conn $ del [key]
  return ()

deleteValue' :: Connection -> String -> IO ()
deleteValue' conn key = deleteValue conn (BS.pack key)

getValue :: Connection -> BS.ByteString -> IO (Maybe BS.ByteString)
getValue conn key = do
  v <- runRedis conn $ get key
  return $ case v of
    (Left _) -> Nothing
    (Right res) -> case res of
      Nothing   -> Nothing
      (Just v') -> Just v'

getValue' :: Connection -> String -> IO (Maybe BS.ByteString)
getValue' conn = getValue conn . encodeUtf8 . T.pack

getJsonValue' :: (FromJSON v) => Connection -> String -> IO (Either String v)
getJsonValue' conn key = do
  v' <- getValue' conn key
  case v' of
    Nothing -> return $ Left "No data!"
    (Just v) -> do
      return $ eitherDecode (BS.fromStrict v)

getOrCacheJsonValue :: (FromJSON v, ToJSON v) => Connection -> Maybe Integer -> String -> Handler (Maybe v) -> Handler (Either String v)
getOrCacheJsonValue conn timeout key valueF = let
  cacheF :: (ToJSON v) => Handler (Maybe v) -> Handler (Either String v)
  cacheF valueF' = do
    v' <- valueF'
    case v' of
      Nothing  -> return $ Left "Failed to get value!"
      (Just v) -> do
        _ <- liftIO $ cacheValue conn ((encodeUtf8 . T.pack) key) (BS.toStrict $ encode v) timeout
        return $ Right v
  in do
    cachedData <- liftIO $ getJsonValue' conn key
    case cachedData of
      (Left _)  -> cacheF valueF
      (Right v) -> return $ Right v
