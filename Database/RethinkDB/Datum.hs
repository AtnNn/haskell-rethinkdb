module Database.RethinkDB.Datum where

import Data.Aeson
import Data.Aeson.Types

data Datum =
  Value Value |
  Other -- TODO
  deriving (Eq, Show)

-- TODO custom show for datum

class FromDatum a where
  parseDatum :: Datum -> Parser a

fromDatum :: Datum -> Result a
fromDatum = undefined

class ToDatum a where
  toDatum :: a -> Datum