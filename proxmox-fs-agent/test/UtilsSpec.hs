module UtilsSpec (spec) where

import qualified Data.Map   as M
import           Parser
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
  describe "Args build test" $ do
    it "VNCless build" $ do
      constructVMArgs [OtherArgs "123", OtherArgs "--key=value", OtherArgs "@"] `shouldBe` "123 --key=value @"
    it "VNC build" $ do
      constructVMArgs [VNCArgs "127.0.0.1:5001"] `shouldBe` "-vnc 127.0.0.1:5001"
    it "Dual setup" $ do
      constructVMArgs [OtherArgs "--key=value", VNCArgs "127.0.0.1:1", OtherArgs "build", OtherArgs "123"] `shouldBe` "--key=value -vnc 127.0.0.1:1 build 123"
  describe "Dump settings test" $ do
    it "Core snap dump" $ do
      dumpSettings M.empty `shouldBe` ""
      dumpSettings (M.fromList [(Nothing, ["#this is comment", "args: -vnc 0.0.0.0:1", "name: server", "agent: 1"])]) `shouldBe` "#this is comment\nargs: -vnc 0.0.0.0:1\nname: server\nagent: 1"
    it "Snapshot dump" $ do
      dumpSettings (M.fromList [(Just "init_snap", ["#comment", "name: aboba", "agent: 0"])]) `shouldBe` "[init_snap]\n#comment\nname: aboba\nagent: 0"
    it "Hybrid dump" $ do
      dumpSettings (M.fromList
        [
          (Nothing, ["name: start_state", "agent: 1", "vnc: -vnc 0.0.0.0:1"]),
          (Just "snap_1", ["name: vm-1", "agent: 0", "args: -vnc 127.0.0.1:50"]),
          (Just "snap_2", ["name: last-snap"])
        ]) `shouldBe` "name: start_state\nagent: 1\nvnc: -vnc 0.0.0.0:1\n\n[snap_1]\nname: vm-1\nagent: 0\nargs: -vnc 127.0.0.1:50\n\n[snap_2]\nname: last-snap"
  describe "VNC settings update test" $ do
    it "No VNC root template set test" $ do
      setVNCSettings (VNCArgs "0.0.0.0:50") (M.fromList [(Nothing, ["name: server"])])
        `shouldBe` (Right . M.fromList) [(Nothing, ["args: -vnc 0.0.0.0:50", "name: server"])]
    it "VNC root template update (no args params)" $ do
      setVNCSettings (VNCArgs "0.0.0.0:100") (M.fromList [(Nothing, ["name: server", "args: -vnc 127.0.0.1:1"])])
        `shouldBe` (Right . M.fromList) [(Nothing, ["args: -vnc 0.0.0.0:100", "name: server"])]
    it "VNC multiple modifications" $ do
      setVNCSettings (VNCArgs "0.0.0.0:2")
        (M.fromList [(Nothing, ["name: server1", "args: --value=key -vnc 127.0.0.1:2"]), (Just "init", []), (Just "last", ["name: last", "agent: 0", "args: -vnc 172.0.0.1:5"])])
          `shouldBe` (Right . M.fromList) [(Nothing, ["args: -vnc 0.0.0.0:2 --value=key", "name: server1"]), (Just "init", ["args: -vnc 0.0.0.0:2"]), (Just "last", ["args: -vnc 0.0.0.0:2", "name: last", "agent: 0"])]
