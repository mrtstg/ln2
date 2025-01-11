{-# LANGUAGE OverloadedStrings #-}
module Data.Models.RoleSpec (spec) where

import           Data.Models.Auth.Role
import           Test.Hspec
import           Utils.Auth

spec :: Spec
spec = do
  describe "Admin role filter test" $ do
    it "Positive test" $ do
      adminRoleGranted [RoleDetails "users" "", RoleDetails "admins" "", RoleDetails "???" ""] `shouldBe` True
      adminRoleGranted [RoleDetails "admins" ""] `shouldBe` True
    it "Negative test" $ do
      adminRoleGranted [RoleDetails "Admins" ""] `shouldBe` False
      adminRoleGranted [RoleDetails "users" "", RoleDetails "???" ""] `shouldBe` False
      adminRoleGranted [RoleDetails "users" "admins"] `shouldBe` False
      adminRoleGranted [RoleDetails "admins-123" ""] `shouldBe` False
  describe "Course id extraction test" $ do
    it "Positive tests" $ do
      getUserAdminCourses' [RoleDetails "admins-123" ""] `shouldBe` ["123"]
      getUserAdminCourses' [RoleDetails "members-123" ""] `shouldBe` []

