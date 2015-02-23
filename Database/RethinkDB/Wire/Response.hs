module Database.RethinkDB.Wire.Response where
import Prelude (Maybe(..), Eq, Show)
import Database.RethinkDB.Wire
data ResponseType = SUCCESS_ATOM | SUCCESS_SEQUENCE | SUCCESS_PARTIAL | SUCCESS_FEED | WAIT_COMPLETE | SUCCESS_ATOM_FEED | CLIENT_ERROR | COMPILE_ERROR | RUNTIME_ERROR
  deriving (Eq, Show)
instance WireValue ResponseType where
  toWire SUCCESS_ATOM = 1
  toWire SUCCESS_SEQUENCE = 2
  toWire SUCCESS_PARTIAL = 3
  toWire SUCCESS_FEED = 5
  toWire WAIT_COMPLETE = 4
  toWire SUCCESS_ATOM_FEED = 6
  toWire CLIENT_ERROR = 16
  toWire COMPILE_ERROR = 17
  toWire RUNTIME_ERROR = 18
  fromWire 1 = Just SUCCESS_ATOM
  fromWire 2 = Just SUCCESS_SEQUENCE
  fromWire 3 = Just SUCCESS_PARTIAL
  fromWire 5 = Just SUCCESS_FEED
  fromWire 4 = Just WAIT_COMPLETE
  fromWire 6 = Just SUCCESS_ATOM_FEED
  fromWire 16 = Just CLIENT_ERROR
  fromWire 17 = Just COMPILE_ERROR
  fromWire 18 = Just RUNTIME_ERROR
  fromWire _ = Nothing


