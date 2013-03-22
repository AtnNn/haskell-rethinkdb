module Database.RethinkDB.Objects where

import Data.Default
import Data.Int
import Data.Text as Text

type Key = Text

-- | A database, referenced by name
data Database = Database {
  databaseName :: Text
  } deriving (Eq, Ord)

instance Show Database where
  show (Database d) = show d

-- | Options used to create a table
data TableCreateOptions = TableCreateOptions {
  tableDataCenter :: Maybe Text,
  tableCacheSize :: Maybe Int64,
  tablePrimaryKey :: Maybe Key
  }

instance Default TableCreateOptions where
  def = TableCreateOptions Nothing Nothing Nothing

-- | A table description
data Table = Table {
  tableDatabase :: Maybe Database, -- ^ when Nothing, use the connection's database
  tableName :: Text
  } deriving (Eq, Ord)

instance Show Table where
  show (Table db' nam) =
    maybe "" (\(Database d) -> Text.unpack d++".") db' ++ Text.unpack nam
