{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( listYMLFiles
  , findYMLByName
  , findYMLByName'
  , createYMLPath
  , constructPostgreStringFromEnv
  , validateStandCheck
) where
import           Data.Models.Stand
import           Data.Models.StandCheck
import qualified Data.Text              as T
import           System.Directory
import           System.Environment     (lookupEnv)
import           System.FilePath        (addExtension, combine, dropExtension,
                                         takeExtension, takeFileName)

standContainerExists :: StandData -> String -> Either String ()
standContainerExists (StandData { getStandContainers = containers }) containerName =
  case containerName `elem` map getContainerName containers of
    True  -> return ()
    False -> Left $ containerName ++ " not found in stand!"

standFilePathValid :: FilePath -> Either String ()
standFilePathValid "" = Left "File path must be specified!"
standFilePathValid _  = Right ()

standCommandValid :: T.Text -> Either String ()
standCommandValid "" = Left "Command to container must be specified!"
standCommandValid _  = Right()

validateStandCheck :: StandData -> [StandCheckStage] -> Either String ()
validateStandCheck d = helper where
  helper :: [StandCheckStage] -> Either String ()
  helper []                                        = Right ()
  helper ((CopyFile containerName _ filePath):ls)  = standContainerExists d containerName >> standFilePathValid filePath >> helper ls
  helper ((ExecuteCommand containerName cmd _ _):ls) = standContainerExists d containerName >> standCommandValid cmd >> helper ls

constructPostgreStringFromEnv :: IO (Maybe String)
constructPostgreStringFromEnv = do
  dbUser'' <- lookupEnv "POSTGRES_USER"
  dbPass'' <- lookupEnv "POSTGRES_PASSWORD"
  dbName'' <- lookupEnv "POSTGRES_DB"
  dbHost'' <- lookupEnv "POSTGRES_HOST"
  dbPort'' <- lookupEnv "POSTGRES_PORT"
  return $ do
    dbUser' <- dbUser''
    dbPass' <- dbPass''
    dbName' <- dbName''
    dbHost' <- dbHost''
    dbPort' <- dbPort''
    return $ "user=" <>
      dbUser' <>
      " password=" <>
      dbPass' <>
      " host=" <>
      dbHost' <>
      " port=" <>
      dbPort' <>
      " dbname=" <>
      dbName'

listYMLFiles :: FilePath -> IO [FilePath]
listYMLFiles f = do
  files' <- listDirectory f
  return $ filter ((==".yml") . takeExtension) files'

findYMLByName :: FilePath -> String -> IO (Maybe FilePath)
findYMLByName f name = do
  files <- listYMLFiles f
  case filter ((==name) . takeFileName . dropExtension) files of
    []    -> return Nothing
    _smth -> return (Just $ createYMLPath f name)

findYMLByName' :: FilePath -> String -> IO (Maybe ())
findYMLByName' f name = do
  files <- listYMLFiles f
  case filter ((==name) . takeFileName . dropExtension) files of
    []    -> return Nothing
    _smth -> return $ Just ()

createYMLPath :: FilePath -> String -> FilePath
createYMLPath f name = f `combine` addExtension name "yml"
