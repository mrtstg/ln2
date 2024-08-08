module Crud.DisplayNumbers
  ( freeDisplayNumbers
  , reserveDisplay
  ) where

import           Data.Maybe       (fromMaybe)
import           Data.Text        (Text)
import           Database.Persist
import           Foundation
import           Yesod.Persist

displayNumbers :: [Int]
-- taking up to 30000 port
displayNumbers = filter excludePorts [1..24100] where
  excludePorts :: Int -> Bool
  -- 8006 port - proxmox UI
  excludePorts 2106 = False
  -- 8000 port - proxmox FS agent
  excludePorts 2100 = False
  excludePorts _    = True

suggestDisplayNumbers :: Handler [Int]
suggestDisplayNumbers = do
  displays <- runDB $ selectList [] []
  let displayNumbers' = map (\(Entity _ e) -> takenDisplayNumber e) displays
  return $ filter (`notElem` displayNumbers') displayNumbers

freeDisplayNumbers :: [Int] -> Handler ()
freeDisplayNumbers displayNums = runDB $ do
  deleteWhere [TakenDisplayNumber <-. displayNums]

reserveDisplay :: Maybe Int -> Text -> Handler (Either String (Entity TakenDisplay))
reserveDisplay vmId displayComment = let
  iterateDisplays :: [Int] -> Handler (Maybe (Entity TakenDisplay))
  iterateDisplays [] = return Nothing
  iterateDisplays (el:els) = do
    portTaken <- runDB $ exists [TakenDisplayNumber ==. el]
    if portTaken then iterateDisplays els else do
      (displayEntity', displayKey) <- runDB $ do
        displayKey <- insert (TakenDisplay el displayComment (fromMaybe 0 vmId))
        displayEntity <- get displayKey
        return (displayEntity, displayKey)
      case displayEntity' of
        Nothing  -> return Nothing
        (Just e) -> return (Just (Entity displayKey e))
  in do
    suggestedDisplays <- suggestDisplayNumbers
    displayRes <- iterateDisplays suggestedDisplays
    case displayRes of
      Nothing  -> return $ Left "No free displays!"
      (Just e) -> return $ Right e
