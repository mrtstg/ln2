{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Utils
  ( listYMLFiles
  , findYMLByName
  , findYMLByName'
  , createYMLPath
  , constructPostgreStringFromEnv
  , createRedisConnectionFromEnv
  , validateStandCheck
) where
import           Data.Models.Stand
import           Data.Models.StandCheck
import qualified Data.Text              as T
import           Database.Redis
import           System.Directory
import           System.Environment     (lookupEnv)
import           System.FilePath        (addExtension, combine, dropExtension,
                                         takeExtension, takeFileName)

createRedisConnectionFromEnv :: IO (Maybe Connection)
createRedisConnectionFromEnv = do
  redisHost'' <- lookupEnv "REDIS_HOST"
  redisPort'' <- lookupEnv "REDIS_PORT"
  case (redisHost'', redisPort'') of
    (Just redisHost', Just redisPort') -> do
      connection <- connect $ defaultConnectInfo { connectHost = redisHost', connectPort = (PortNumber . read) redisPort' }
      return $ Just connection
    _anyOther -> return Nothing

standContainerExists :: StandData -> String -> Either String ()
standContainerExists (StandData { getStandContainers = containers }) containerName =
  if containerName `elem` map getContainerName containers then return () else Left $ containerName ++ " not found in stand!"

stackVariableDeclared :: T.Text -> [T.Text] -> Either String ()
stackVariableDeclared v vs = if v `elem` vs then Right () else Left $ "Variable " <> T.unpack v <> " is not declared!"

standFilePathValid :: FilePath -> Either String ()
standFilePathValid "" = Left "File path must be specified!"
standFilePathValid _  = Right ()

standCommandValid :: T.Text -> Either String ()
standCommandValid "" = Left "Command to container must be specified!"
standCommandValid _  = Right()

validateStandCheck :: StandData -> [StandCheckStage] -> Either String ()
validateStandCheck d = helper [] where
  helper :: [T.Text] -> [StandCheckStage] -> Either String ()
  helper _ []                                        = Right ()
  helper stack ((CopyFile { .. }):ls)  = do
    () <- standContainerExists d getStageTarget
    () <- standFilePathValid getStageFilePath
    helper stack ls
  helper stack ((ExecuteCommand { .. }):ls) = do
    () <- standContainerExists d getStageTarget
    () <- standCommandValid getStageCommand
    case getStageRecordVariable of
      Nothing  -> helper stack ls
      (Just v) -> helper (v:stack) ls
  -- TODO: forbid variable overflow
  helper stack ((CompareVariables { .. }):ls) = do
    () <- stackVariableDeclared getStageFirstV stack
    () <- stackVariableDeclared getStageSecondV stack
    () <- validateStandCheck d getStagePositiveActions
    () <- validateStandCheck d getStageNegativeActions
    helper stack ls
  -- TODO: empty var check
  helper stack ((DeclareVariable varName _):ls) = helper (varName:stack) ls
  helper stack ((AddPoints { .. }):ls) = do
    () <- if getStagePointsAmount > 0 then Right () else Left "Points cant be negative!"
    helper stack ls
  helper stack ((CompareLatestStatusCode { .. }):ls) = do
    () <- validateStandCheck d getStagePositiveActions
    () <- validateStandCheck d getStageNegativeActions
    helper stack ls
  helper stack (StopCheck:ls) = helper stack ls
  helper stack ((DisplayMessage { .. }):ls) = do
    () <- if T.null getStandMessage then Left "Empty message for display!" else Right ()
    helper stack ls
  helper stack ((DisplayVariable { ..}):ls) = do
    () <- if T.null getStandMessage then Left "Empty message for display!" else Right ()
    () <- stackVariableDeclared getStandVariableName stack
    helper stack ls


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
