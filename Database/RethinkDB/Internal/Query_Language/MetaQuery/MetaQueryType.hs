{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Internal.Query_Language.MetaQuery.MetaQueryType (MetaQueryType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data MetaQueryType = CREATE_DB
                   | DROP_DB
                   | LIST_DBS
                   | CREATE_TABLE
                   | DROP_TABLE
                   | LIST_TABLES
                   deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable MetaQueryType
 
instance Prelude'.Bounded MetaQueryType where
  minBound = CREATE_DB
  maxBound = LIST_TABLES
 
instance P'.Default MetaQueryType where
  defaultValue = CREATE_DB
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe MetaQueryType
toMaybe'Enum 1 = Prelude'.Just CREATE_DB
toMaybe'Enum 2 = Prelude'.Just DROP_DB
toMaybe'Enum 3 = Prelude'.Just LIST_DBS
toMaybe'Enum 4 = Prelude'.Just CREATE_TABLE
toMaybe'Enum 5 = Prelude'.Just DROP_TABLE
toMaybe'Enum 6 = Prelude'.Just LIST_TABLES
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum MetaQueryType where
  fromEnum CREATE_DB = 1
  fromEnum DROP_DB = 2
  fromEnum LIST_DBS = 3
  fromEnum CREATE_TABLE = 4
  fromEnum DROP_TABLE = 5
  fromEnum LIST_TABLES = 6
  toEnum
   = P'.fromMaybe
      (Prelude'.error
        "hprotoc generated code: toEnum failure for type Database.RethinkDB.Internal.Query_Language.MetaQuery.MetaQueryType")
      . toMaybe'Enum
  succ CREATE_DB = DROP_DB
  succ DROP_DB = LIST_DBS
  succ LIST_DBS = CREATE_TABLE
  succ CREATE_TABLE = DROP_TABLE
  succ DROP_TABLE = LIST_TABLES
  succ _
   = Prelude'.error
      "hprotoc generated code: succ failure for type Database.RethinkDB.Internal.Query_Language.MetaQuery.MetaQueryType"
  pred DROP_DB = CREATE_DB
  pred LIST_DBS = DROP_DB
  pred CREATE_TABLE = LIST_DBS
  pred DROP_TABLE = CREATE_TABLE
  pred LIST_TABLES = DROP_TABLE
  pred _
   = Prelude'.error
      "hprotoc generated code: pred failure for type Database.RethinkDB.Internal.Query_Language.MetaQuery.MetaQueryType"
 
instance P'.Wire MetaQueryType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB MetaQueryType
 
instance P'.MessageAPI msg' (msg' -> MetaQueryType) MetaQueryType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum MetaQueryType where
  reflectEnum
   = [(1, "CREATE_DB", CREATE_DB), (2, "DROP_DB", DROP_DB), (3, "LIST_DBS", LIST_DBS), (4, "CREATE_TABLE", CREATE_TABLE),
      (5, "DROP_TABLE", DROP_TABLE), (6, "LIST_TABLES", LIST_TABLES)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Query_Language.MetaQuery.MetaQueryType") ["Database", "RethinkDB", "Internal"]
        ["Query_Language", "MetaQuery"]
        "MetaQueryType")
      ["Database", "RethinkDB", "Internal", "Query_Language", "MetaQuery", "MetaQueryType.hs"]
      [(1, "CREATE_DB"), (2, "DROP_DB"), (3, "LIST_DBS"), (4, "CREATE_TABLE"), (5, "DROP_TABLE"), (6, "LIST_TABLES")]