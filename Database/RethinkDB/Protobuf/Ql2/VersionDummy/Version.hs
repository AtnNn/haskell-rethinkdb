{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Protobuf.Ql2.VersionDummy.Version (Version(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Version = V0_1
             | V0_2
             deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Version
 
instance Prelude'.Bounded Version where
  minBound = V0_1
  maxBound = V0_2
 
instance P'.Default Version where
  defaultValue = V0_1
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe Version
toMaybe'Enum 1063369270 = Prelude'.Just V0_1
toMaybe'Enum 1915781601 = Prelude'.Just V0_2
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Version where
  fromEnum V0_1 = 1063369270
  fromEnum V0_2 = 1915781601
  toEnum
   = P'.fromMaybe
      (Prelude'.error "hprotoc generated code: toEnum failure for type Database.RethinkDB.Protobuf.Ql2.VersionDummy.Version")
      . toMaybe'Enum
  succ V0_1 = V0_2
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Database.RethinkDB.Protobuf.Ql2.VersionDummy.Version"
  pred V0_2 = V0_1
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Database.RethinkDB.Protobuf.Ql2.VersionDummy.Version"
 
instance P'.Wire Version where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Version
 
instance P'.MessageAPI msg' (msg' -> Version) Version where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Version where
  reflectEnum = [(1063369270, "V0_1", V0_1), (1915781601, "V0_2", V0_2)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Ql2.VersionDummy.Version") ["Database", "RethinkDB", "Protobuf"] ["Ql2", "VersionDummy"] "Version")
      ["Database", "RethinkDB", "Protobuf", "Ql2", "VersionDummy", "Version.hs"]
      [(1063369270, "V0_1"), (1915781601, "V0_2")]