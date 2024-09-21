{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Utils
  ( listYMLFiles
  , findYMLByName
  , findYMLByName'
  , createYMLPath
  , validateStandCheck
) where
import           Data.Models.Stand
import           Data.Models.StandCheck
import qualified Data.Text              as T
import           System.Directory
import           System.FilePath        (addExtension, combine, dropExtension,
                                         takeExtension, takeFileName)

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

validateStandCheck :: StandData -> [T.Text] -> [StandCheckStage] -> Either String ()
validateStandCheck d = helper where
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
    () <- validateStandCheck d stack getStagePositiveActions
    () <- validateStandCheck d stack getStageNegativeActions
    helper stack ls
  -- TODO: empty var check
  helper stack ((DeclareVariable varName _):ls) = helper (varName:stack) ls
  helper stack ((AddPoints { .. }):ls) = do
    () <- if getStagePointsAmount > 0 then Right () else Left "Points cant be negative!"
    helper stack ls
  helper stack ((SetPointsGate { .. }):ls) = do
    () <- if getStageNeededPoints > -1 then Right () else Left "Needed points cant be negative!"
    helper stack ls
  helper stack ((CompareLatestStatusCode { .. }):ls) = do
    () <- validateStandCheck d stack getStagePositiveActions
    () <- validateStandCheck d stack getStageNegativeActions
    helper stack ls
  helper stack (StopCheck:ls) = helper stack ls
  helper stack (AcceptCheck:ls) = helper stack ls
  helper stack ((DisplayMessage { .. }):ls) = do
    () <- if T.null getStageMessage then Left "Empty message for display!" else Right ()
    helper stack ls
  helper stack ((DisplayVariable { ..}):ls) = do
    () <- if T.null getStageMessage then Left "Empty message for display!" else Right ()
    () <- stackVariableDeclared getStageVariableName stack
    helper stack ls

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
