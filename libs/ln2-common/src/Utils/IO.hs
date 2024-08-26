module Utils.IO
  ( retryIOEither
  , retryIOEither'
  ) where

import           Control.Concurrent

type Tries = Int
type Timeout = Int

retryIOEither' :: IO (Either a b) -> IO (Either a b)
retryIOEither' = retryIOEither 5 100000

retryIOEither :: Tries -> Timeout -> IO (Either a b) -> IO (Either a b)
retryIOEither tries timeout f = do
  res <- f
  case res of
    (Left err) -> do
      if tries == 0 then return (Left err) else do
        () <- threadDelay timeout
        retryIOEither (tries - 1) timeout f
    (Right res) -> return (Right res)
