{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Protobuf.Ql2.Response.ResponseType (ResponseType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data ResponseType = SUCCESS_ATOM
                  | SUCCESS_SEQUENCE
                  | SUCCESS_PARTIAL
                  | WAIT_COMPLETE
                  | CLIENT_ERROR
                  | COMPILE_ERROR
                  | RUNTIME_ERROR
                  deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ResponseType
 
instance Prelude'.Bounded ResponseType where
  minBound = SUCCESS_ATOM
  maxBound = RUNTIME_ERROR
 
instance P'.Default ResponseType where
  defaultValue = SUCCESS_ATOM
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe ResponseType
toMaybe'Enum 1 = Prelude'.Just SUCCESS_ATOM
toMaybe'Enum 2 = Prelude'.Just SUCCESS_SEQUENCE
toMaybe'Enum 3 = Prelude'.Just SUCCESS_PARTIAL
toMaybe'Enum 4 = Prelude'.Just WAIT_COMPLETE
toMaybe'Enum 16 = Prelude'.Just CLIENT_ERROR
toMaybe'Enum 17 = Prelude'.Just COMPILE_ERROR
toMaybe'Enum 18 = Prelude'.Just RUNTIME_ERROR
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum ResponseType where
  fromEnum SUCCESS_ATOM = 1
  fromEnum SUCCESS_SEQUENCE = 2
  fromEnum SUCCESS_PARTIAL = 3
  fromEnum WAIT_COMPLETE = 4
  fromEnum CLIENT_ERROR = 16
  fromEnum COMPILE_ERROR = 17
  fromEnum RUNTIME_ERROR = 18
  toEnum
   = P'.fromMaybe
      (Prelude'.error "hprotoc generated code: toEnum failure for type Database.RethinkDB.Protobuf.Ql2.Response.ResponseType")
      . toMaybe'Enum
  succ SUCCESS_ATOM = SUCCESS_SEQUENCE
  succ SUCCESS_SEQUENCE = SUCCESS_PARTIAL
  succ SUCCESS_PARTIAL = WAIT_COMPLETE
  succ WAIT_COMPLETE = CLIENT_ERROR
  succ CLIENT_ERROR = COMPILE_ERROR
  succ COMPILE_ERROR = RUNTIME_ERROR
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Database.RethinkDB.Protobuf.Ql2.Response.ResponseType"
  pred SUCCESS_SEQUENCE = SUCCESS_ATOM
  pred SUCCESS_PARTIAL = SUCCESS_SEQUENCE
  pred WAIT_COMPLETE = SUCCESS_PARTIAL
  pred CLIENT_ERROR = WAIT_COMPLETE
  pred COMPILE_ERROR = CLIENT_ERROR
  pred RUNTIME_ERROR = COMPILE_ERROR
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Database.RethinkDB.Protobuf.Ql2.Response.ResponseType"
 
instance P'.Wire ResponseType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB ResponseType
 
instance P'.MessageAPI msg' (msg' -> ResponseType) ResponseType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum ResponseType where
  reflectEnum
   = [(1, "SUCCESS_ATOM", SUCCESS_ATOM), (2, "SUCCESS_SEQUENCE", SUCCESS_SEQUENCE), (3, "SUCCESS_PARTIAL", SUCCESS_PARTIAL),
      (4, "WAIT_COMPLETE", WAIT_COMPLETE), (16, "CLIENT_ERROR", CLIENT_ERROR), (17, "COMPILE_ERROR", COMPILE_ERROR),
      (18, "RUNTIME_ERROR", RUNTIME_ERROR)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Ql2.Response.ResponseType") ["Database", "RethinkDB", "Protobuf"] ["Ql2", "Response"] "ResponseType")
      ["Database", "RethinkDB", "Protobuf", "Ql2", "Response", "ResponseType.hs"]
      [(1, "SUCCESS_ATOM"), (2, "SUCCESS_SEQUENCE"), (3, "SUCCESS_PARTIAL"), (4, "WAIT_COMPLETE"), (16, "CLIENT_ERROR"),
       (17, "COMPILE_ERROR"), (18, "RUNTIME_ERROR")]