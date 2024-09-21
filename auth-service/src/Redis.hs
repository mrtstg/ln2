{-# LANGUAGE OverloadedStrings #-}
module Redis
  ( rewriteAuthToken
  , rewriteAuthToken'
  , deleteAuthToken
  ) where

import           Control.Monad      (when)
import           Data.Either
import           Data.Maybe
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           Database.Redis

deleteAuthToken :: Connection -> T.Text -> IO ()
deleteAuthToken conn login = do
  let loginBS = encodeUtf8 $ "token-" <> login
  runRedis conn $ do
    prevToken <- get loginBS
    case prevToken of
      (Right r) -> case r of
        (Just oldToken) -> do
          _ <- del [oldToken]
          return ()
        Nothing -> return ()
      (Left _) -> return ()
    _ <- del [loginBS]
    return ()

rewriteAuthToken' :: Maybe Integer -> Connection -> T.Text -> String -> IO ()
rewriteAuthToken' timeout conn login token = do
  let loginBS = encodeUtf8 login
  let tokenBS = (encodeUtf8 . T.pack) token
  let tokenKey = encodeUtf8 $ "token-" <> login
  runRedis conn $ do
    prevToken <- get tokenKey
    when (isRight prevToken) $ case prevToken of
      (Right r) -> when (isJust r) $ case r of
        (Just oldToken) -> do
          _ <- del [oldToken]
          return ()

    _ <- setOpts tokenKey tokenBS (SetOpts timeout Nothing Nothing)
    _ <- setOpts tokenBS loginBS (SetOpts timeout Nothing Nothing)
    return ()

-- TODO: returning result?
rewriteAuthToken :: Connection -> T.Text -> String -> IO ()
rewriteAuthToken = rewriteAuthToken' (Just 7200)
