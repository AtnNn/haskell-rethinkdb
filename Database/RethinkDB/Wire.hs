module Database.RethinkDB.Wire where
class WireValue a where
  toWire :: a -> Int
  fromWire :: Int -> Maybe a
