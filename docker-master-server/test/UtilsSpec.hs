{-# LANGUAGE OverloadedStrings #-}
module UtilsSpec (spec) where

import           Data.Models.Stand
import           Data.Models.StandCheck
import           Test.Hspec
import           Utils

sampleStand :: StandData
sampleStand = StandData [
  ContainerData
    "postgres"
    "postgres:16-alpine"
    Nothing
    Nothing
    Nothing
    []
    Nothing
  ] []

sampleStand' :: StandData
sampleStand' = StandData [
  ContainerData "python" "python:3.11" Nothing Nothing Nothing [] Nothing,
  ContainerData "postgres" "postgres:16-alpine" Nothing Nothing Nothing [] (Just 10)
  ] []

spec :: Spec
spec = describe "Stand validation test" $ do
  it "File path validation [non-empty path filter]" $ do
    validateStandCheck sampleStand [
      CopyFile "postgres" "SELECT * FROM table;" ""
      ] `shouldBe` Left "File path must be specified!"

    validateStandCheck sampleStand [
      CopyFile "postgres" "SELECT * FROM table;" "/script.sql"
      ] `shouldBe` Right ()
  it "Container command validation [non-empty command]" $ do
    validateStandCheck sampleStand [
      ExecuteCommand "postgres" "" False False Nothing
      ] `shouldBe` Left "Command to container must be specified!"
  it "Single container and step test" $ do
    validateStandCheck sampleStand [
      CopyFile "postgresql" "" "/tmp/a"
      ] `shouldBe` Left "postgresql not found in stand!"
    validateStandCheck sampleStand [
      CopyFile "postgres" "" "/tmp/a"
      ] `shouldBe` Right ()
  it "Multiple containers test" $ do
    validateStandCheck sampleStand' [
      CopyFile "postgresql" "" "/tmp/a"
      ] `shouldBe` Left "postgresql not found in stand!"
    validateStandCheck sampleStand' [
      CopyFile "postgres" "" "/tmp/a"
      ] `shouldBe` Right ()
  it "Single container, multiple steps test" $ do
    validateStandCheck sampleStand [
      CopyFile "postgres" "SELECT * FROM user;" "/tmp/a.sql",
      ExecuteCommand "postgresql" "psql -f /tmp/a.sql --csv" True False Nothing
      ] `shouldBe` Left "postgresql not found in stand!"

    validateStandCheck sampleStand [
      CopyFile "postgres" "SELECT * FROM user;" "/tmp/a.sql",
      ExecuteCommand "postgresql" "psql -f /tmp/a.sql --csv" True False Nothing,
      CopyFile "aaa" "" "/tmp/a.sql"
      ] `shouldBe` Left "postgresql not found in stand!"

    validateStandCheck sampleStand [
      CopyFile "postgres" "SELECT * FROM user;" "/tmp/a.sql",
      ExecuteCommand "postgres" "psql -f /tmp/a.sql --csv" True False Nothing
      ] `shouldBe` Right ()
  it "Multiple containers, multiple steps test" $ do
    -- TODO: more tests
    validateStandCheck sampleStand' [
      CopyFile "postgres" "SELECT * FROM user;" "/tmp/a.sql",
      ExecuteCommand "postgresql" "psql -f /tmp/a.sql --csv" True False Nothing
      ] `shouldBe` Left "postgresql not found in stand!"

    validateStandCheck sampleStand' [
      CopyFile "postgres" "SELECT * FROM user;" "/tmp/a.sql",
      ExecuteCommand "postgresql" "psql -f /tmp/a.sql --csv" True False Nothing,
      CopyFile "aaa" "" "/tmp/a.sql"
      ] `shouldBe` Left "postgresql not found in stand!"

    validateStandCheck sampleStand' [
      CopyFile "postgres" "SELECT * FROM user;" "/tmp/a.sql",
      ExecuteCommand "postgres" "psql -f /tmp/a.sql --csv" True False Nothing
      ] `shouldBe` Right ()
  it "Variables check tests" $ do
    validateStandCheck sampleStand' [
      ExecuteCommand "postgres" "psql -f /tmp/b.sql --csv" True False (Just "taskRes"),
      ExecuteCommand "postgres" "psql -f /tmp/a.sql --csv" True False (Just "solveRes"),
      CompareResults "solveRes" "taskRes" 1
      ] `shouldBe` Right ()
    validateStandCheck sampleStand' [
      ExecuteCommand "postgres" "psql -f /tmp/b.sql --csv" True False (Just "taskRes"),
      CompareResults "solveRes" "taskRes" 1
      ] `shouldBe` Left "Variable solveRes is not declared!"
    validateStandCheck sampleStand' [
      ExecuteCommand "postgres" "psql -f /tmp/a.sql --csv" True False (Just "solveRes"),
      CompareResults "solveRes" "taskRes" 1
      ] `shouldBe` Left "Variable taskRes is not declared!"
  it "Compare score tests" $ do
    validateStandCheck sampleStand' [
      ExecuteCommand "postgres" "psql -f /tmp/a.sql --csv" True False (Just "solveRes"),
      ExecuteCommand "postgres" "psql -f /tmp/b.sql --csv" True False (Just "taskRes"),
      CompareResults "solveRes" "taskRes" 0
      ] `shouldBe` Left "Score can't be negative or zero!"
    validateStandCheck sampleStand' [
      ExecuteCommand "postgres" "psql -f /tmp/a.sql --csv" True False (Just "solveRes"),
      ExecuteCommand "postgres" "psql -f /tmp/b.sql --csv" True False (Just "taskRes"),
      CompareResults "solveRes" "taskRes" (-1)
      ] `shouldBe` Left "Score can't be negative or zero!"
