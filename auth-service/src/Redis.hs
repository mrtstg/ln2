module Redis
  ( cacheValue
  , cacheValue'
  , deleteValue
  , getValue
  , getValue'
  , rewriteAuthToken
  ) where

import           Control.Monad         (when)
import qualified Data.ByteString.Char8 as BS
import           Data.Either
import           Data.Maybe
import qualified Data.Text             as T
import           Database.Redis

-- TODO: login timeout customization
-- TODO: returning result?
rewriteAuthToken :: Connection -> T.Text -> String -> IO ()
rewriteAuthToken conn login token = do
  let loginBS = (BS.pack . T.unpack) login
  let tokenBS = BS.pack token
  let tokenKey = BS.pack $ "token-" <> T.unpack login
  runRedis conn $ do
    prevToken <- get tokenKey
    when (isRight prevToken) $ case prevToken of
      (Right r) -> when (isJust r) $ case r of
        (Just oldToken) -> do
          _ <- del [oldToken]
          return ()

    _ <- setOpts tokenKey tokenBS (SetOpts (Just 7200) Nothing Nothing)
    _ <- setOpts tokenBS loginBS (SetOpts (Just 7200) Nothing Nothing)
    return ()

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

getValue :: Connection -> BS.ByteString -> IO (Maybe BS.ByteString)
getValue conn key = do
  v <- runRedis conn $ get key
  return $ case v of
    (Left _) -> Nothing
    (Right res) -> case res of
      Nothing   -> Nothing
      (Just v') -> Just v'

getValue' :: Connection -> String -> IO (Maybe BS.ByteString)
getValue' conn = getValue conn . BS.pack
