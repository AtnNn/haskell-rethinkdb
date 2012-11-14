{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Internal.Query_Language.Builtin.Comparison (Comparison(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Comparison = EQ
                | NE
                | LT
                | LE
                | GT
                | GE
                deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Comparison
 
instance Prelude'.Bounded Comparison where
  minBound = EQ
  maxBound = GE
 
instance P'.Default Comparison where
  defaultValue = EQ
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe Comparison
toMaybe'Enum 1 = Prelude'.Just EQ
toMaybe'Enum 2 = Prelude'.Just NE
toMaybe'Enum 3 = Prelude'.Just LT
toMaybe'Enum 4 = Prelude'.Just LE
toMaybe'Enum 5 = Prelude'.Just GT
toMaybe'Enum 6 = Prelude'.Just GE
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Comparison where
  fromEnum EQ = 1
  fromEnum NE = 2
  fromEnum LT = 3
  fromEnum LE = 4
  fromEnum GT = 5
  fromEnum GE = 6
  toEnum
   = P'.fromMaybe
      (Prelude'.error
        "hprotoc generated code: toEnum failure for type Database.RethinkDB.Internal.Query_Language.Builtin.Comparison")
      . toMaybe'Enum
  succ EQ = NE
  succ NE = LT
  succ LT = LE
  succ LE = GT
  succ GT = GE
  succ _
   = Prelude'.error "hprotoc generated code: succ failure for type Database.RethinkDB.Internal.Query_Language.Builtin.Comparison"
  pred NE = EQ
  pred LT = NE
  pred LE = LT
  pred GT = LE
  pred GE = GT
  pred _
   = Prelude'.error "hprotoc generated code: pred failure for type Database.RethinkDB.Internal.Query_Language.Builtin.Comparison"
 
instance P'.Wire Comparison where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Comparison
 
instance P'.MessageAPI msg' (msg' -> Comparison) Comparison where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Comparison where
  reflectEnum = [(1, "EQ", EQ), (2, "NE", NE), (3, "LT", LT), (4, "LE", LE), (5, "GT", GT), (6, "GE", GE)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Query_Language.Builtin.Comparison") ["Database", "RethinkDB", "Internal"] ["Query_Language", "Builtin"]
        "Comparison")
      ["Database", "RethinkDB", "Internal", "Query_Language", "Builtin", "Comparison.hs"]
      [(1, "EQ"), (2, "NE"), (3, "LT"), (4, "LE"), (5, "GT"), (6, "GE")]