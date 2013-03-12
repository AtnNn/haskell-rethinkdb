module Database.RethinkDB.Objects where

-- | A database, referenced by name
data Database = Database {
  databaseName :: String
  } deriving (Eq, Ord)

instance Show Database where
  show (Database d) = show d
