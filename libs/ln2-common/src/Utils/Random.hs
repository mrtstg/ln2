module Utils.Random (randomIOString) where

import           Data.UnixTime
import           System.Random

randomIOString :: Int -> IO String
randomIOString n = do
  gen <- mkStdGen . fromIntegral . utMicroSeconds <$> getUnixTime
  return (take n $ randomRs ('a', 'z') gen)
