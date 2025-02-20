module Utils
  ( getVMOptionsFromFile
  , getVMArgs
  , dumpSettings
  , replaceVMArgs
  , getVMOptions
  , constructVMArgs
  ) where

import           Data.Bifunctor   (second)
import           Data.List        (isPrefixOf)
import qualified Data.List        as L
import qualified Data.Map         as M
import           Data.Maybe       (isJust, isNothing)
import           Parser
import qualified System.IO.Strict as S

constructVMArgs :: [VMArgs] -> String
constructVMArgs args' = if head result == ' ' then drop 1 result else result  where
  result = helper [] args'
  helper :: String -> [VMArgs] -> String
  helper acc ((OtherArgs v):args)     = helper (acc ++ " " ++ v) args
  helper acc ((VNCArgs address):args) = helper (acc ++ " -vnc " ++ address) args
  helper acc []                       = acc

getVMOptionsFromFile :: FilePath -> IO (M.Map (Maybe String) [String])
getVMOptionsFromFile p = do
  content <- S.readFile p
  return $ getVMOptions content

getVMOptions :: String -> M.Map (Maybe String) [String]
getVMOptions = f M.empty Nothing . L.lines where
  f :: M.Map (Maybe String) [String] -> Maybe String -> [String] -> M.Map (Maybe String) [String]
  f m _ [] = m
  f m snapName (line:ls) = do
    if "[" `isPrefixOf` line && last line == ']' then do
      f m ((Just . drop 1 . init) line) ls
    else do
      if not (null line) then do
        case M.lookup snapName m of
          Nothing          -> f (M.insert snapName [line] m) snapName ls
          (Just oldValues) -> f (M.insert snapName (oldValues ++ [line]) m) snapName ls
      else f m snapName ls

getVMArgs :: M.Map (Maybe String) [String] -> M.Map (Maybe String) (Maybe String)
getVMArgs opts = M.fromList $ map (second f) (M.toList opts) where
  f :: [String] -> Maybe String
  f opts' = case filter ("args: " `isPrefixOf`) opts' of
    []       -> Nothing
    (line:_) -> Just line

dumpSettings :: M.Map (Maybe String) [String] -> String
dumpSettings paramsMap = helper (L.intercalate "\n" (coreSnapshotParams paramsList)) (filter (isJust . fst) paramsList) where
  paramsList = M.toList paramsMap

  headOr :: [[String]] -> [String] -> [String]
  headOr [] d    = d
  headOr (l:_) _ = l

  coreSnapshotParams :: [(Maybe String, [String])] -> [String]
  coreSnapshotParams = flip headOr [] . map snd . filter (isNothing . fst)

  helper ::  String -> [(Maybe String, [String])] -> String
  helper acc ((Just snapName, paramsLines):ls) = helper (acc ++ (if (not . null) acc then "\n" else "") ++ "[" ++ snapName ++ "]\n" ++ L.intercalate "\n" paramsLines) ls
  helper _ ((Nothing, _):_) = error "Found core snapshot params!"
  helper acc [] = acc

--dumpSettings :: FilePath -> [String] -> IO ()
--dumpSettings path opts = writeFile path (L.intercalate "\n" opts <> "\n")

replaceVMArgs :: String -> [String] -> [String]
replaceVMArgs newArgs opts = newArgs:filter (not . isPrefixOf "args: ") opts
