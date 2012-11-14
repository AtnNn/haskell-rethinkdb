{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Internal.Query_Language.Response.StatusCode (StatusCode(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data StatusCode = SUCCESS_EMPTY
                | SUCCESS_JSON
                | SUCCESS_PARTIAL
                | SUCCESS_STREAM
                | BROKEN_CLIENT
                | BAD_QUERY
                | RUNTIME_ERROR
                deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable StatusCode
 
instance Prelude'.Bounded StatusCode where
  minBound = SUCCESS_EMPTY
  maxBound = RUNTIME_ERROR
 
instance P'.Default StatusCode where
  defaultValue = SUCCESS_EMPTY
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe StatusCode
toMaybe'Enum 0 = Prelude'.Just SUCCESS_EMPTY
toMaybe'Enum 1 = Prelude'.Just SUCCESS_JSON
toMaybe'Enum 2 = Prelude'.Just SUCCESS_PARTIAL
toMaybe'Enum 3 = Prelude'.Just SUCCESS_STREAM
toMaybe'Enum 101 = Prelude'.Just BROKEN_CLIENT
toMaybe'Enum 102 = Prelude'.Just BAD_QUERY
toMaybe'Enum 103 = Prelude'.Just RUNTIME_ERROR
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum StatusCode where
  fromEnum SUCCESS_EMPTY = 0
  fromEnum SUCCESS_JSON = 1
  fromEnum SUCCESS_PARTIAL = 2
  fromEnum SUCCESS_STREAM = 3
  fromEnum BROKEN_CLIENT = 101
  fromEnum BAD_QUERY = 102
  fromEnum RUNTIME_ERROR = 103
  toEnum
   = P'.fromMaybe
      (Prelude'.error
        "hprotoc generated code: toEnum failure for type Database.RethinkDB.Internal.Query_Language.Response.StatusCode")
      . toMaybe'Enum
  succ SUCCESS_EMPTY = SUCCESS_JSON
  succ SUCCESS_JSON = SUCCESS_PARTIAL
  succ SUCCESS_PARTIAL = SUCCESS_STREAM
  succ SUCCESS_STREAM = BROKEN_CLIENT
  succ BROKEN_CLIENT = BAD_QUERY
  succ BAD_QUERY = RUNTIME_ERROR
  succ _
   = Prelude'.error "hprotoc generated code: succ failure for type Database.RethinkDB.Internal.Query_Language.Response.StatusCode"
  pred SUCCESS_JSON = SUCCESS_EMPTY
  pred SUCCESS_PARTIAL = SUCCESS_JSON
  pred SUCCESS_STREAM = SUCCESS_PARTIAL
  pred BROKEN_CLIENT = SUCCESS_STREAM
  pred BAD_QUERY = BROKEN_CLIENT
  pred RUNTIME_ERROR = BAD_QUERY
  pred _
   = Prelude'.error "hprotoc generated code: pred failure for type Database.RethinkDB.Internal.Query_Language.Response.StatusCode"
 
instance P'.Wire StatusCode where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB StatusCode
 
instance P'.MessageAPI msg' (msg' -> StatusCode) StatusCode where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum StatusCode where
  reflectEnum
   = [(0, "SUCCESS_EMPTY", SUCCESS_EMPTY), (1, "SUCCESS_JSON", SUCCESS_JSON), (2, "SUCCESS_PARTIAL", SUCCESS_PARTIAL),
      (3, "SUCCESS_STREAM", SUCCESS_STREAM), (101, "BROKEN_CLIENT", BROKEN_CLIENT), (102, "BAD_QUERY", BAD_QUERY),
      (103, "RUNTIME_ERROR", RUNTIME_ERROR)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Query_Language.Response.StatusCode") ["Database", "RethinkDB", "Internal"]
        ["Query_Language", "Response"]
        "StatusCode")
      ["Database", "RethinkDB", "Internal", "Query_Language", "Response", "StatusCode.hs"]
      [(0, "SUCCESS_EMPTY"), (1, "SUCCESS_JSON"), (2, "SUCCESS_PARTIAL"), (3, "SUCCESS_STREAM"), (101, "BROKEN_CLIENT"),
       (102, "BAD_QUERY"), (103, "RUNTIME_ERROR")]