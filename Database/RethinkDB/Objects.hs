module Database.RethinkDB.Objects (
  Database(..),
  TableCreateOptions(..),
  IndexCreateOptions(..),
  Table(..),
  Datum,
  Key
  ) where

import Data.Default (def, Default)
import Data.Text as Text
import Data.Aeson (Value)

type Key = Text

-- | A database, referenced by name
data Database = Database {
  databaseName :: Text
  } deriving (Eq, Ord)

instance Show Database where
  show (Database d) = show d

-- | Options used to create a table
data TableCreateOptions = TableCreateOptions {
  tableDataCenter :: Maybe Text
  }

instance Default TableCreateOptions where
  def = TableCreateOptions Nothing

-- | Options used to create an index
data IndexCreateOptions = IndexCreateOptions {
  indexMulti :: Maybe Bool
  }

instance Default IndexCreateOptions where
  def = IndexCreateOptions Nothing

-- | A table description
data Table = Table {
  tableDatabase :: Maybe Database, -- ^ when Nothing, use the connection's database
  tableName :: Text,
  tablePrimaryKey :: Maybe Key
  } deriving (Eq, Ord)

instance Show Table where
  show (Table db' nam mkey) =
    maybe "" (\(Database d) -> Text.unpack d++".") db' ++ Text.unpack nam ++
    maybe "" (\k -> "[" ++ show k ++ "]") mkey

type Datum = Value