module Database.RethinkDB.Wire.Frame where
import Prelude (Maybe(..), Eq, Show)
import Database.RethinkDB.Wire
data FrameType = POS | OPT
  deriving (Eq, Show)
instance WireValue FrameType where
  toWire POS = 1
  toWire OPT = 2
  fromWire 1 = Just POS
  fromWire 2 = Just OPT
  fromWire _ = Nothing


