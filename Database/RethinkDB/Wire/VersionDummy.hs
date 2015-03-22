module Database.RethinkDB.Wire.VersionDummy where
import Prelude (Maybe(..), Eq, Show)
import Database.RethinkDB.Wire
data Version = V0_1 | V0_2 | V0_3 | V0_4
  deriving (Eq, Show)
instance WireValue Version where
  toWire V0_1 = 0x3f61ba36
  toWire V0_2 = 0x723081e1
  toWire V0_3 = 0x5f75e83e
  toWire V0_4 = 0x400c2d20
  fromWire 0x3f61ba36 = Just V0_1
  fromWire 0x723081e1 = Just V0_2
  fromWire 0x5f75e83e = Just V0_3
  fromWire 0x400c2d20 = Just V0_4
  fromWire _ = Nothing


data Protocol = PROTOBUF | JSON
  deriving (Eq, Show)
instance WireValue Protocol where
  toWire PROTOBUF = 0x271ffc41
  toWire JSON = 0x7e6970c7
  fromWire 0x271ffc41 = Just PROTOBUF
  fromWire 0x7e6970c7 = Just JSON
  fromWire _ = Nothing


