{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Internal.Query_Language.Builtin.BuiltinType (BuiltinType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data BuiltinType = NOT
                 | GETATTR
                 | IMPLICIT_GETATTR
                 | HASATTR
                 | IMPLICIT_HASATTR
                 | PICKATTRS
                 | IMPLICIT_PICKATTRS
                 | MAPMERGE
                 | ARRAYAPPEND
                 | SLICE
                 | ADD
                 | SUBTRACT
                 | MULTIPLY
                 | DIVIDE
                 | MODULO
                 | COMPARE
                 | FILTER
                 | MAP
                 | CONCATMAP
                 | ORDERBY
                 | DISTINCT
                 | LENGTH
                 | UNION
                 | NTH
                 | STREAMTOARRAY
                 | ARRAYTOSTREAM
                 | REDUCE
                 | GROUPEDMAPREDUCE
                 | ANY
                 | ALL
                 | RANGE
                 | IMPLICIT_WITHOUT
                 | WITHOUT
                 deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable BuiltinType
 
instance Prelude'.Bounded BuiltinType where
  minBound = NOT
  maxBound = WITHOUT
 
instance P'.Default BuiltinType where
  defaultValue = NOT
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe BuiltinType
toMaybe'Enum 1 = Prelude'.Just NOT
toMaybe'Enum 2 = Prelude'.Just GETATTR
toMaybe'Enum 3 = Prelude'.Just IMPLICIT_GETATTR
toMaybe'Enum 4 = Prelude'.Just HASATTR
toMaybe'Enum 5 = Prelude'.Just IMPLICIT_HASATTR
toMaybe'Enum 6 = Prelude'.Just PICKATTRS
toMaybe'Enum 7 = Prelude'.Just IMPLICIT_PICKATTRS
toMaybe'Enum 8 = Prelude'.Just MAPMERGE
toMaybe'Enum 9 = Prelude'.Just ARRAYAPPEND
toMaybe'Enum 11 = Prelude'.Just SLICE
toMaybe'Enum 14 = Prelude'.Just ADD
toMaybe'Enum 15 = Prelude'.Just SUBTRACT
toMaybe'Enum 16 = Prelude'.Just MULTIPLY
toMaybe'Enum 17 = Prelude'.Just DIVIDE
toMaybe'Enum 18 = Prelude'.Just MODULO
toMaybe'Enum 19 = Prelude'.Just COMPARE
toMaybe'Enum 20 = Prelude'.Just FILTER
toMaybe'Enum 21 = Prelude'.Just MAP
toMaybe'Enum 22 = Prelude'.Just CONCATMAP
toMaybe'Enum 23 = Prelude'.Just ORDERBY
toMaybe'Enum 24 = Prelude'.Just DISTINCT
toMaybe'Enum 26 = Prelude'.Just LENGTH
toMaybe'Enum 27 = Prelude'.Just UNION
toMaybe'Enum 28 = Prelude'.Just NTH
toMaybe'Enum 29 = Prelude'.Just STREAMTOARRAY
toMaybe'Enum 30 = Prelude'.Just ARRAYTOSTREAM
toMaybe'Enum 31 = Prelude'.Just REDUCE
toMaybe'Enum 32 = Prelude'.Just GROUPEDMAPREDUCE
toMaybe'Enum 35 = Prelude'.Just ANY
toMaybe'Enum 36 = Prelude'.Just ALL
toMaybe'Enum 37 = Prelude'.Just RANGE
toMaybe'Enum 38 = Prelude'.Just IMPLICIT_WITHOUT
toMaybe'Enum 39 = Prelude'.Just WITHOUT
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum BuiltinType where
  fromEnum NOT = 1
  fromEnum GETATTR = 2
  fromEnum IMPLICIT_GETATTR = 3
  fromEnum HASATTR = 4
  fromEnum IMPLICIT_HASATTR = 5
  fromEnum PICKATTRS = 6
  fromEnum IMPLICIT_PICKATTRS = 7
  fromEnum MAPMERGE = 8
  fromEnum ARRAYAPPEND = 9
  fromEnum SLICE = 11
  fromEnum ADD = 14
  fromEnum SUBTRACT = 15
  fromEnum MULTIPLY = 16
  fromEnum DIVIDE = 17
  fromEnum MODULO = 18
  fromEnum COMPARE = 19
  fromEnum FILTER = 20
  fromEnum MAP = 21
  fromEnum CONCATMAP = 22
  fromEnum ORDERBY = 23
  fromEnum DISTINCT = 24
  fromEnum LENGTH = 26
  fromEnum UNION = 27
  fromEnum NTH = 28
  fromEnum STREAMTOARRAY = 29
  fromEnum ARRAYTOSTREAM = 30
  fromEnum REDUCE = 31
  fromEnum GROUPEDMAPREDUCE = 32
  fromEnum ANY = 35
  fromEnum ALL = 36
  fromEnum RANGE = 37
  fromEnum IMPLICIT_WITHOUT = 38
  fromEnum WITHOUT = 39
  toEnum
   = P'.fromMaybe
      (Prelude'.error
        "hprotoc generated code: toEnum failure for type Database.RethinkDB.Internal.Query_Language.Builtin.BuiltinType")
      . toMaybe'Enum
  succ NOT = GETATTR
  succ GETATTR = IMPLICIT_GETATTR
  succ IMPLICIT_GETATTR = HASATTR
  succ HASATTR = IMPLICIT_HASATTR
  succ IMPLICIT_HASATTR = PICKATTRS
  succ PICKATTRS = IMPLICIT_PICKATTRS
  succ IMPLICIT_PICKATTRS = MAPMERGE
  succ MAPMERGE = ARRAYAPPEND
  succ ARRAYAPPEND = SLICE
  succ SLICE = ADD
  succ ADD = SUBTRACT
  succ SUBTRACT = MULTIPLY
  succ MULTIPLY = DIVIDE
  succ DIVIDE = MODULO
  succ MODULO = COMPARE
  succ COMPARE = FILTER
  succ FILTER = MAP
  succ MAP = CONCATMAP
  succ CONCATMAP = ORDERBY
  succ ORDERBY = DISTINCT
  succ DISTINCT = LENGTH
  succ LENGTH = UNION
  succ UNION = NTH
  succ NTH = STREAMTOARRAY
  succ STREAMTOARRAY = ARRAYTOSTREAM
  succ ARRAYTOSTREAM = REDUCE
  succ REDUCE = GROUPEDMAPREDUCE
  succ GROUPEDMAPREDUCE = ANY
  succ ANY = ALL
  succ ALL = RANGE
  succ RANGE = IMPLICIT_WITHOUT
  succ IMPLICIT_WITHOUT = WITHOUT
  succ _
   = Prelude'.error "hprotoc generated code: succ failure for type Database.RethinkDB.Internal.Query_Language.Builtin.BuiltinType"
  pred GETATTR = NOT
  pred IMPLICIT_GETATTR = GETATTR
  pred HASATTR = IMPLICIT_GETATTR
  pred IMPLICIT_HASATTR = HASATTR
  pred PICKATTRS = IMPLICIT_HASATTR
  pred IMPLICIT_PICKATTRS = PICKATTRS
  pred MAPMERGE = IMPLICIT_PICKATTRS
  pred ARRAYAPPEND = MAPMERGE
  pred SLICE = ARRAYAPPEND
  pred ADD = SLICE
  pred SUBTRACT = ADD
  pred MULTIPLY = SUBTRACT
  pred DIVIDE = MULTIPLY
  pred MODULO = DIVIDE
  pred COMPARE = MODULO
  pred FILTER = COMPARE
  pred MAP = FILTER
  pred CONCATMAP = MAP
  pred ORDERBY = CONCATMAP
  pred DISTINCT = ORDERBY
  pred LENGTH = DISTINCT
  pred UNION = LENGTH
  pred NTH = UNION
  pred STREAMTOARRAY = NTH
  pred ARRAYTOSTREAM = STREAMTOARRAY
  pred REDUCE = ARRAYTOSTREAM
  pred GROUPEDMAPREDUCE = REDUCE
  pred ANY = GROUPEDMAPREDUCE
  pred ALL = ANY
  pred RANGE = ALL
  pred IMPLICIT_WITHOUT = RANGE
  pred WITHOUT = IMPLICIT_WITHOUT
  pred _
   = Prelude'.error "hprotoc generated code: pred failure for type Database.RethinkDB.Internal.Query_Language.Builtin.BuiltinType"
 
instance P'.Wire BuiltinType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB BuiltinType
 
instance P'.MessageAPI msg' (msg' -> BuiltinType) BuiltinType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum BuiltinType where
  reflectEnum
   = [(1, "NOT", NOT), (2, "GETATTR", GETATTR), (3, "IMPLICIT_GETATTR", IMPLICIT_GETATTR), (4, "HASATTR", HASATTR),
      (5, "IMPLICIT_HASATTR", IMPLICIT_HASATTR), (6, "PICKATTRS", PICKATTRS), (7, "IMPLICIT_PICKATTRS", IMPLICIT_PICKATTRS),
      (8, "MAPMERGE", MAPMERGE), (9, "ARRAYAPPEND", ARRAYAPPEND), (11, "SLICE", SLICE), (14, "ADD", ADD),
      (15, "SUBTRACT", SUBTRACT), (16, "MULTIPLY", MULTIPLY), (17, "DIVIDE", DIVIDE), (18, "MODULO", MODULO),
      (19, "COMPARE", COMPARE), (20, "FILTER", FILTER), (21, "MAP", MAP), (22, "CONCATMAP", CONCATMAP), (23, "ORDERBY", ORDERBY),
      (24, "DISTINCT", DISTINCT), (26, "LENGTH", LENGTH), (27, "UNION", UNION), (28, "NTH", NTH),
      (29, "STREAMTOARRAY", STREAMTOARRAY), (30, "ARRAYTOSTREAM", ARRAYTOSTREAM), (31, "REDUCE", REDUCE),
      (32, "GROUPEDMAPREDUCE", GROUPEDMAPREDUCE), (35, "ANY", ANY), (36, "ALL", ALL), (37, "RANGE", RANGE),
      (38, "IMPLICIT_WITHOUT", IMPLICIT_WITHOUT), (39, "WITHOUT", WITHOUT)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Query_Language.Builtin.BuiltinType") ["Database", "RethinkDB", "Internal"]
        ["Query_Language", "Builtin"]
        "BuiltinType")
      ["Database", "RethinkDB", "Internal", "Query_Language", "Builtin", "BuiltinType.hs"]
      [(1, "NOT"), (2, "GETATTR"), (3, "IMPLICIT_GETATTR"), (4, "HASATTR"), (5, "IMPLICIT_HASATTR"), (6, "PICKATTRS"),
       (7, "IMPLICIT_PICKATTRS"), (8, "MAPMERGE"), (9, "ARRAYAPPEND"), (11, "SLICE"), (14, "ADD"), (15, "SUBTRACT"),
       (16, "MULTIPLY"), (17, "DIVIDE"), (18, "MODULO"), (19, "COMPARE"), (20, "FILTER"), (21, "MAP"), (22, "CONCATMAP"),
       (23, "ORDERBY"), (24, "DISTINCT"), (26, "LENGTH"), (27, "UNION"), (28, "NTH"), (29, "STREAMTOARRAY"), (30, "ARRAYTOSTREAM"),
       (31, "REDUCE"), (32, "GROUPEDMAPREDUCE"), (35, "ANY"), (36, "ALL"), (37, "RANGE"), (38, "IMPLICIT_WITHOUT"), (39, "WITHOUT")]