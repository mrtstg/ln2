module Utils
  ( getVMOptionsFromFile
  , getVMArgs
  , dumpSettings
  , replaceVMArgs
  , getVMOptions
  ) where

import           Data.List        (isPrefixOf)
import qualified Data.List        as L
import qualified Data.Map         as M
import qualified System.IO.Strict as S

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

getVMArgs :: [String] -> Maybe String
getVMArgs opts = if null results then Nothing else Just $ head results where
  results :: [String]
  results = filter ("args: " `isPrefixOf`) opts

dumpSettings :: FilePath -> [String] -> IO ()
dumpSettings path opts = writeFile path (L.intercalate "\n" opts <> "\n")

replaceVMArgs :: String -> [String] -> [String]
replaceVMArgs newArgs opts = newArgs:filter (not . isPrefixOf "args: ") opts
