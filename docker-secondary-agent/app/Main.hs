{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           App.Commands
import           App.Parser          (appParser)
import           Control.Monad       (when)
import           LoadEnv
import           Options.Applicative
import           System.Directory    (doesFileExist)
import           System.IO

main :: IO ()
main = do
  -- TODO: is it safe?
  hSetBuffering stdout NoBuffering
  envFileExists <- doesFileExist "./.env"
  when envFileExists $ do
    putStrLn "Loading environment variables!"
    loadEnvFrom "./.env"
  opts <- execParser (info (appParser <**> helper) fullDesc)
  runCommand opts
