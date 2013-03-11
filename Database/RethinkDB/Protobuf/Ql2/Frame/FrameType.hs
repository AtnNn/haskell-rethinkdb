{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Protobuf.Ql2.Frame.FrameType (FrameType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data FrameType = POS
               | OPT
               deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable FrameType
 
instance Prelude'.Bounded FrameType where
  minBound = POS
  maxBound = OPT
 
instance P'.Default FrameType where
  defaultValue = POS
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe FrameType
toMaybe'Enum 1 = Prelude'.Just POS
toMaybe'Enum 2 = Prelude'.Just OPT
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum FrameType where
  fromEnum POS = 1
  fromEnum OPT = 2
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Database.RethinkDB.Protobuf.Ql2.Frame.FrameType")
      . toMaybe'Enum
  succ POS = OPT
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Database.RethinkDB.Protobuf.Ql2.Frame.FrameType"
  pred OPT = POS
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Database.RethinkDB.Protobuf.Ql2.Frame.FrameType"
 
instance P'.Wire FrameType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB FrameType
 
instance P'.MessageAPI msg' (msg' -> FrameType) FrameType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum FrameType where
  reflectEnum = [(1, "POS", POS), (2, "OPT", OPT)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Ql2.Frame.FrameType") ["Database", "RethinkDB", "Protobuf"] ["Ql2", "Frame"] "FrameType")
      ["Database", "RethinkDB", "Protobuf", "Ql2", "Frame", "FrameType.hs"]
      [(1, "POS"), (2, "OPT")]