{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Internal.Query_Language.WriteQuery.WriteQueryType (WriteQueryType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data WriteQueryType = UPDATE
                    | DELETE
                    | MUTATE
                    | INSERT
                    | FOREACH
                    | POINTUPDATE
                    | POINTDELETE
                    | POINTMUTATE
                    deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable WriteQueryType
 
instance Prelude'.Bounded WriteQueryType where
  minBound = UPDATE
  maxBound = POINTMUTATE
 
instance P'.Default WriteQueryType where
  defaultValue = UPDATE
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe WriteQueryType
toMaybe'Enum 1 = Prelude'.Just UPDATE
toMaybe'Enum 2 = Prelude'.Just DELETE
toMaybe'Enum 3 = Prelude'.Just MUTATE
toMaybe'Enum 4 = Prelude'.Just INSERT
toMaybe'Enum 6 = Prelude'.Just FOREACH
toMaybe'Enum 7 = Prelude'.Just POINTUPDATE
toMaybe'Enum 8 = Prelude'.Just POINTDELETE
toMaybe'Enum 9 = Prelude'.Just POINTMUTATE
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum WriteQueryType where
  fromEnum UPDATE = 1
  fromEnum DELETE = 2
  fromEnum MUTATE = 3
  fromEnum INSERT = 4
  fromEnum FOREACH = 6
  fromEnum POINTUPDATE = 7
  fromEnum POINTDELETE = 8
  fromEnum POINTMUTATE = 9
  toEnum
   = P'.fromMaybe
      (Prelude'.error
        "hprotoc generated code: toEnum failure for type Database.RethinkDB.Internal.Query_Language.WriteQuery.WriteQueryType")
      . toMaybe'Enum
  succ UPDATE = DELETE
  succ DELETE = MUTATE
  succ MUTATE = INSERT
  succ INSERT = FOREACH
  succ FOREACH = POINTUPDATE
  succ POINTUPDATE = POINTDELETE
  succ POINTDELETE = POINTMUTATE
  succ _
   = Prelude'.error
      "hprotoc generated code: succ failure for type Database.RethinkDB.Internal.Query_Language.WriteQuery.WriteQueryType"
  pred DELETE = UPDATE
  pred MUTATE = DELETE
  pred INSERT = MUTATE
  pred FOREACH = INSERT
  pred POINTUPDATE = FOREACH
  pred POINTDELETE = POINTUPDATE
  pred POINTMUTATE = POINTDELETE
  pred _
   = Prelude'.error
      "hprotoc generated code: pred failure for type Database.RethinkDB.Internal.Query_Language.WriteQuery.WriteQueryType"
 
instance P'.Wire WriteQueryType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB WriteQueryType
 
instance P'.MessageAPI msg' (msg' -> WriteQueryType) WriteQueryType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum WriteQueryType where
  reflectEnum
   = [(1, "UPDATE", UPDATE), (2, "DELETE", DELETE), (3, "MUTATE", MUTATE), (4, "INSERT", INSERT), (6, "FOREACH", FOREACH),
      (7, "POINTUPDATE", POINTUPDATE), (8, "POINTDELETE", POINTDELETE), (9, "POINTMUTATE", POINTMUTATE)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Query_Language.WriteQuery.WriteQueryType") ["Database", "RethinkDB", "Internal"]
        ["Query_Language", "WriteQuery"]
        "WriteQueryType")
      ["Database", "RethinkDB", "Internal", "Query_Language", "WriteQuery", "WriteQueryType.hs"]
      [(1, "UPDATE"), (2, "DELETE"), (3, "MUTATE"), (4, "INSERT"), (6, "FOREACH"), (7, "POINTUPDATE"), (8, "POINTDELETE"),
       (9, "POINTMUTATE")]