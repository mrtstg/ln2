module UtilsSpec (spec) where

import qualified Data.Map   as M
import           Test.Hspec
import           Utils

spec :: Spec
spec = do
  describe "Config scrap test" $ do
    it "Snapshotless test" $ do
      getVMOptions "args: -vnc 0.0.0.0:5\nagent: 1\nname: 123" `shouldBe` M.fromList
        [
          (Nothing, ["args: -vnc 0.0.0.0:5", "agent: 1", "name: 123"])
        ]
    it "One snapshot test" $ do
      getVMOptions "[snap_name]\nargs: -vnc 127.0.0.1:100\nagent: 0" `shouldBe` M.fromList
        [
          (Just "snap_name", ["args: -vnc 127.0.0.1:100", "agent: 0"])
        ]
    it "Hybrid test" $ do
      getVMOptions "agent: 1\nargs: -vnc 0.0.0.0:5\ncpu: host\nname: vm\n[init]\nname: vm-start\nagent: 0\n\n\n[finish]\nname: vm-finish" `shouldBe` M.fromList
        [
          (Nothing, ["agent: 1", "args: -vnc 0.0.0.0:5", "cpu: host", "name: vm"]),
          (Just "init", ["name: vm-start", "agent: 0"]),
          (Just "finish", ["name: vm-finish"])
        ]
