module UtilsSpec (spec) where

import           Test.Hspec
import           Utils

spec :: Spec
spec = do
  describe "Beginning and end strip test" $ do
    it "Newlines (and other symbols) are getting stripped at beginning of line" $ do
      formatString' "\n\nHello world!" `shouldBe` "Hello world!"
      formatString' " Hello world!" `shouldBe` "Hello world!"
      formatString' "    Hello world!" `shouldBe` "Hello world!"
      formatString' "\tHello world!" `shouldBe` "Hello world!"
    it "Symbols are cut off at end of line" $ do
      formatString' "Hello world!\n" `shouldBe` "Hello world!"
      formatString' "String one\nString two\n" `shouldBe` "String one\nString two"
      formatString' "Multiple\n\n" `shouldBe` "Multiple"
      formatString' "Combined \n\t" `shouldBe` "Combined"
  describe "Middle symbols processing test" $ do
    it "Multiple symbols is unified" $ do
      formatString' "First line\n\nSecond line" `shouldBe` "First line\nSecond line"
      formatString' "Extremely       long     spaces" `shouldBe` "Extremely long spaces"
    it "Tabs are translated into spaces" $ do
      formatString' "Word\tafter tab" `shouldBe` "Word after tab"
      formatString' "1\t\t2\t\t3\nAlice\tBob\tMark" `shouldBe` "1 2 3\nAlice Bob Mark"
