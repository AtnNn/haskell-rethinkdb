module Database.RethinkDB.Objects (
  Database(..),
  TableCreateOptions(..),
  IndexCreateOptions(..),
  Table(..),
  JSON(..),
  Datum,
  Key,
  Index(..)
  ) where

import Data.Default (def, Default)
import qualified Data.Text as Text
import Data.Text.Lazy (unpack)
import Data.Text (Text)
import Data.Aeson (Value(..), FromJSON(..))
import Data.Aeson.Encode (encodeToTextBuilder)
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

type Datum = Value

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

data Index =
  PrimaryKey |
  Index Key