module Utils
  ( getVMOptionsFromFile
  , getVMArgs
  , dumpSettings
  , replaceVMArgs
  ) where

import           Data.List        (isPrefixOf)
import qualified Data.List        as L
import qualified System.IO.Strict as S

getVMOptionsFromFile :: FilePath -> IO [String]
getVMOptionsFromFile p = do
  content <- S.readFile p
  return $ getVMOptions content

getVMOptions :: String -> [String]
getVMOptions = filter (not . null) . L.lines

getVMArgs :: [String] -> Maybe String
getVMArgs opts = if null results then Nothing else Just $ head results where
  results :: [String]
  results = filter ("args: " `isPrefixOf`) opts

dumpSettings :: FilePath -> [String] -> IO ()
dumpSettings path opts = writeFile path (L.intercalate "\n" opts <> "\n")

replaceVMArgs :: String -> [String] -> [String]
replaceVMArgs newArgs opts = newArgs:filter (not . isPrefixOf "args: ") opts
