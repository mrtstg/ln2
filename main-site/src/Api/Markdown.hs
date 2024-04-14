{-# LANGUAGE OverloadedStrings #-}
module Api.Markdown (
  convertMarkdown',
  checkMarkdown,
  MDResult(..)
  ) where

import           Control.Exception          (catch)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import           Network.HTTP.Simple
import           System.Environment
import           Yesod.Core                 (liftIO)

newtype MDResponse = MDResponse T.Text deriving (Show, Eq)

instance FromJSON MDResponse where
  parseJSON = withObject "MDResponse" $ \v -> MDResponse <$> v .: "data"

data MDResult a = MDResult !a | MDError !String | NoApiURL deriving (Show, Eq)

mdHandler' :: HttpException -> IO (Either HttpException (MDResult a))
mdHandler' _ = return $ Right (MDError "Unknown error!")

getMDApiURL :: IO (MDResult T.Text)
getMDApiURL = do
  apiUrl' <- lookupEnv "MD_SERVICE_URL"
  case apiUrl' of
    Nothing -> do
      liftIO $ putStrLn "No markdown API URL provided!"
      return NoApiURL
    Just apiUrl -> return $ MDResult (T.pack apiUrl)

convertMarkdown' :: T.Text -> IO (MDResult T.Text)
convertMarkdown' mdData = do
  r <- runExceptT (convertMarkdown mdData) `catch` mdHandler'
  case r of
    ~(Right v) -> return v

convertMarkdown :: T.Text -> ExceptT HttpException IO (MDResult T.Text)
convertMarkdown mdData = do
  apiUrl' <- liftIO getMDApiURL
  case apiUrl' of
    (MDResult apiUrl) -> do
      let payload = object ["data" .= mdData]
      let reqString = "POST " <> apiUrl <> "/parse"
      request <- parseRequest (T.unpack reqString)
      let requestData = setRequestBodyJSON payload request
      response <- httpBS requestData
      let responseBody = getResponseBody response
      case getResponseStatusCode response of
        400 -> return $ MDError "Failed to parse!"
        200 -> do
          let parseRes = (eitherDecode . LBS.fromStrict) responseBody
          case parseRes of
            (Left e)                    -> return $ MDError e
            (Right (MDResponse mdText)) -> return $ MDResult mdText
        _unexpectedCode -> return $ MDError "Unexpected code!"
    other -> return other

checkMarkdown = undefined
