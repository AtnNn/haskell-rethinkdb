module Database.RethinkDB.Wire.Datum where
import Prelude (Maybe(..), Int)
import Database.RethinkDB.Wire
data DatumType = R_NULL | R_BOOL | R_NUM | R_STR | R_ARRAY | R_OBJECT | R_JSON
instance WireValue DatumType where
  toWire R_NULL = 1
  toWire R_BOOL = 2
  toWire R_NUM = 3
  toWire R_STR = 4
  toWire R_ARRAY = 5
  toWire R_OBJECT = 6
  toWire R_JSON = 7
  fromWire 1 = Just R_NULL
  fromWire 2 = Just R_BOOL
  fromWire 3 = Just R_NUM
  fromWire 4 = Just R_STR
  fromWire 5 = Just R_ARRAY
  fromWire 6 = Just R_OBJECT
  fromWire 7 = Just R_JSON
  fromWire _ = Nothing


