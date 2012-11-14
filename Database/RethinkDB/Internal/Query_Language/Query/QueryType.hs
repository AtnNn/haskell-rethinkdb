{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Internal.Query_Language.Query.QueryType (QueryType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data QueryType = READ
               | WRITE
               | CONTINUE
               | STOP
               | META
               deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable QueryType
 
instance Prelude'.Bounded QueryType where
  minBound = READ
  maxBound = META
 
instance P'.Default QueryType where
  defaultValue = READ
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe QueryType
toMaybe'Enum 1 = Prelude'.Just READ
toMaybe'Enum 2 = Prelude'.Just WRITE
toMaybe'Enum 3 = Prelude'.Just CONTINUE
toMaybe'Enum 4 = Prelude'.Just STOP
toMaybe'Enum 5 = Prelude'.Just META
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum QueryType where
  fromEnum READ = 1
  fromEnum WRITE = 2
  fromEnum CONTINUE = 3
  fromEnum STOP = 4
  fromEnum META = 5
  toEnum
   = P'.fromMaybe
      (Prelude'.error "hprotoc generated code: toEnum failure for type Database.RethinkDB.Internal.Query_Language.Query.QueryType")
      . toMaybe'Enum
  succ READ = WRITE
  succ WRITE = CONTINUE
  succ CONTINUE = STOP
  succ STOP = META
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Database.RethinkDB.Internal.Query_Language.Query.QueryType"
  pred WRITE = READ
  pred CONTINUE = WRITE
  pred STOP = CONTINUE
  pred META = STOP
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Database.RethinkDB.Internal.Query_Language.Query.QueryType"
 
instance P'.Wire QueryType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB QueryType
 
instance P'.MessageAPI msg' (msg' -> QueryType) QueryType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum QueryType where
  reflectEnum = [(1, "READ", READ), (2, "WRITE", WRITE), (3, "CONTINUE", CONTINUE), (4, "STOP", STOP), (5, "META", META)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Query_Language.Query.QueryType") ["Database", "RethinkDB", "Internal"] ["Query_Language", "Query"]
        "QueryType")
      ["Database", "RethinkDB", "Internal", "Query_Language", "Query", "QueryType.hs"]
      [(1, "READ"), (2, "WRITE"), (3, "CONTINUE"), (4, "STOP"), (5, "META")]