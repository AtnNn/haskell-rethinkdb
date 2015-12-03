module Database.RethinkDB.Wire.Query where
import Prelude (Maybe(..), Eq, Show)
import Database.RethinkDB.Wire
data QueryType = START | CONTINUE | STOP | NOREPLY_WAIT | SERVER_INFO
  deriving (Eq, Show)
instance WireValue QueryType where
  toWire START = 1
  toWire CONTINUE = 2
  toWire STOP = 3
  toWire NOREPLY_WAIT = 4
  toWire SERVER_INFO = 5
  fromWire 1 = Just START
  fromWire 2 = Just CONTINUE
  fromWire 3 = Just STOP
  fromWire 4 = Just NOREPLY_WAIT
  fromWire 5 = Just SERVER_INFO
  fromWire _ = Nothing


