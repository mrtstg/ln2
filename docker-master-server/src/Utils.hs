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
  helper stack ((CopyFile containerName _ filePath):ls)  = do
    () <- standContainerExists d containerName
    () <- standFilePathValid filePath
    helper stack ls
  helper stack ((ExecuteCommand containerName cmd _ _ rValue):ls) = do
    () <- standContainerExists d containerName
    () <- standCommandValid cmd
    case rValue of
      Nothing  -> helper stack ls
      (Just v) -> helper (v:stack) ls
  -- TODO: score upper bound
  -- TODO: forbid variable overflow
  helper stack ((CompareResults fst' snd' score):ls) = do
    () <- stackVariableDeclared fst' stack
    () <- stackVariableDeclared snd' stack
    () <- if score > 0 then Right () else Left "Score can't be negative or zero!"
    helper stack ls
  -- TODO: empty var check
  helper stack ((DeclareVariable varName _):ls) = helper (varName:stack) ls

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
