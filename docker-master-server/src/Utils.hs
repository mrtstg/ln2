module Utils
  ( listYMLFiles
  , findYMLByName
  , findYMLByName'
  , createYMLPath
  , constructPostgreStringFromEnv
) where
import           System.Directory
import           System.Environment (lookupEnv)
import           System.FilePath    (addExtension, combine, dropExtension,
                                     takeExtension, takeFileName)

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
