module Database.RethinkDB.Wire.Query where
import Prelude (Maybe(..), Int)
import Database.RethinkDB.Wire
data QueryType = START | CONTINUE | STOP | NOREPLY_WAIT
instance WireValue QueryType where
  toWire START = 1
  toWire CONTINUE = 2
  toWire STOP = 3
  toWire NOREPLY_WAIT = 4
  fromWire 1 = Just START
  fromWire 2 = Just CONTINUE
  fromWire 3 = Just STOP
  fromWire 4 = Just NOREPLY_WAIT
  fromWire _ = Nothing


