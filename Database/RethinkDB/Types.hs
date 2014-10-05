module Database.RethinkDB.Objects (
  Database(..),
  TableCreateOptions(..),
  IndexCreateOptions(..),
  Table(..),
  JSON(..),
  Datum(..),
  Key,
  Index(..),
  FromDatum(..),
  fromDatum
  ) where

import Data.Default (def, Default)
import qualified Data.Text as Text
import Data.Text.Lazy (unpack)
import Data.Text (Text, pack)
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Aeson.Types
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.HashMap.Strict as HM
import Data.String
import Data.Monoid (mconcat, (<>))

type Key = Text

-- | A database, referenced by name
data Database = Database {
  databaseName :: Text
  } deriving (Eq, Ord)

instance Show Database where
  show (Database d) = show d

instance IsString Database where
  fromString name = Database $ fromString name

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

instance IsString Table where
  fromString name = Table Nothing (fromString name) Nothing

data Index =
  PrimaryKey |
  Index Key

instance IsString Index where
  fromString = Index . pack