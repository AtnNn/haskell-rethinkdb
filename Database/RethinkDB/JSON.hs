module Database.RethinkDB.JSON where

import Data.Aeson

data JSON = JSON Value deriving Eq

instance Show JSON where
  show (JSON a) = unpack . toLazyText . encodeToTextBuilder $ a

instance FromJSON JSON where
  parseJSON = fmap JSON . parseJSON

instance Ord JSON where
  compare (JSON x) (JSON y) = x <=> y
    where
      Object a <=> Object b =
        compare (HM.keys a) (HM.keys b) <>
        mconcat (map (\k -> (a HM.! k) <=> (b HM.! k) ) (HM.keys a))
      Object _ <=> _ = LT
      _ <=> Object _ = GT
      Array a <=> Array b = compare (fmap JSON a) (fmap JSON b)
      Array _ <=> _ = LT
      _ <=> Array _ = GT
      String a <=> String b = compare a b
      String _ <=> _ = LT
      _ <=> String _ = GT
      Number a <=> Number b = compare a b
      Number _ <=> _ = LT
      _ <=> Number _ = GT
      Bool a <=> Bool b = compare a b
      Bool _ <=> _ = LT
      _ <=> Bool _ = GT
      Null <=> Null = EQ