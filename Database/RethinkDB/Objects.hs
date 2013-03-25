module Database.RethinkDB.Objects where

import Data.Default
import Data.Int
import Data.Text as Text
import Data.Aeson

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
  tableCacheSize :: Maybe Int64
  }

instance Default TableCreateOptions where
  def = TableCreateOptions Nothing Nothing

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

-- | A reference to a document
data Document = Document {
  documentTable :: Table,
  documentKey :: Datum
  } deriving (Eq)

instance Show Document where
  show (Document t k) = show t ++ "[" ++ show k ++ "]"

type Datum = Value
