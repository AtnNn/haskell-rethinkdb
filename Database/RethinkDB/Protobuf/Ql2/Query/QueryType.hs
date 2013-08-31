{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Protobuf.Ql2.Query.QueryType (QueryType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data QueryType = START
               | CONTINUE
               | STOP
               deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable QueryType
 
instance Prelude'.Bounded QueryType where
  minBound = START
  maxBound = STOP
 
instance P'.Default QueryType where
  defaultValue = START
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe QueryType
toMaybe'Enum 1 = Prelude'.Just START
toMaybe'Enum 2 = Prelude'.Just CONTINUE
toMaybe'Enum 3 = Prelude'.Just STOP
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum QueryType where
  fromEnum START = 1
  fromEnum CONTINUE = 2
  fromEnum STOP = 3
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Database.RethinkDB.Protobuf.Ql2.Query.QueryType")
      . toMaybe'Enum
  succ START = CONTINUE
  succ CONTINUE = STOP
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Database.RethinkDB.Protobuf.Ql2.Query.QueryType"
  pred CONTINUE = START
  pred STOP = CONTINUE
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Database.RethinkDB.Protobuf.Ql2.Query.QueryType"
 
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
  reflectEnum = [(1, "START", START), (2, "CONTINUE", CONTINUE), (3, "STOP", STOP)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Ql2.Query.QueryType") ["Database", "RethinkDB", "Protobuf"] ["Ql2", "Query"] "QueryType")
      ["Database", "RethinkDB", "Protobuf", "Ql2", "Query", "QueryType.hs"]
      [(1, "START"), (2, "CONTINUE"), (3, "STOP")]