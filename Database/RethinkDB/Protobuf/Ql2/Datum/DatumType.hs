{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Protobuf.Ql2.Datum.DatumType (DatumType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data DatumType = R_NULL
               | R_BOOL
               | R_NUM
               | R_STR
               | R_ARRAY
               | R_OBJECT
               | R_JSON
               deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable DatumType
 
instance Prelude'.Bounded DatumType where
  minBound = R_NULL
  maxBound = R_JSON
 
instance P'.Default DatumType where
  defaultValue = R_NULL
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe DatumType
toMaybe'Enum 1 = Prelude'.Just R_NULL
toMaybe'Enum 2 = Prelude'.Just R_BOOL
toMaybe'Enum 3 = Prelude'.Just R_NUM
toMaybe'Enum 4 = Prelude'.Just R_STR
toMaybe'Enum 5 = Prelude'.Just R_ARRAY
toMaybe'Enum 6 = Prelude'.Just R_OBJECT
toMaybe'Enum 7 = Prelude'.Just R_JSON
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum DatumType where
  fromEnum R_NULL = 1
  fromEnum R_BOOL = 2
  fromEnum R_NUM = 3
  fromEnum R_STR = 4
  fromEnum R_ARRAY = 5
  fromEnum R_OBJECT = 6
  fromEnum R_JSON = 7
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Database.RethinkDB.Protobuf.Ql2.Datum.DatumType")
      . toMaybe'Enum
  succ R_NULL = R_BOOL
  succ R_BOOL = R_NUM
  succ R_NUM = R_STR
  succ R_STR = R_ARRAY
  succ R_ARRAY = R_OBJECT
  succ R_OBJECT = R_JSON
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Database.RethinkDB.Protobuf.Ql2.Datum.DatumType"
  pred R_BOOL = R_NULL
  pred R_NUM = R_BOOL
  pred R_STR = R_NUM
  pred R_ARRAY = R_STR
  pred R_OBJECT = R_ARRAY
  pred R_JSON = R_OBJECT
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Database.RethinkDB.Protobuf.Ql2.Datum.DatumType"
 
instance P'.Wire DatumType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB DatumType
 
instance P'.MessageAPI msg' (msg' -> DatumType) DatumType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum DatumType where
  reflectEnum
   = [(1, "R_NULL", R_NULL), (2, "R_BOOL", R_BOOL), (3, "R_NUM", R_NUM), (4, "R_STR", R_STR), (5, "R_ARRAY", R_ARRAY),
      (6, "R_OBJECT", R_OBJECT), (7, "R_JSON", R_JSON)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Ql2.Datum.DatumType") ["Database", "RethinkDB", "Protobuf"] ["Ql2", "Datum"] "DatumType")
      ["Database", "RethinkDB", "Protobuf", "Ql2", "Datum", "DatumType.hs"]
      [(1, "R_NULL"), (2, "R_BOOL"), (3, "R_NUM"), (4, "R_STR"), (5, "R_ARRAY"), (6, "R_OBJECT"), (7, "R_JSON")]