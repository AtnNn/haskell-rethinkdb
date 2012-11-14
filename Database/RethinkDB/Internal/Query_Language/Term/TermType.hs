{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Internal.Query_Language.Term.TermType (TermType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data TermType = JSON_NULL
              | VAR
              | LET
              | CALL
              | IF
              | ERROR
              | NUMBER
              | STRING
              | JSON
              | BOOL
              | ARRAY
              | OBJECT
              | GETBYKEY
              | TABLE
              | JAVASCRIPT
              | IMPLICIT_VAR
              deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable TermType
 
instance Prelude'.Bounded TermType where
  minBound = JSON_NULL
  maxBound = IMPLICIT_VAR
 
instance P'.Default TermType where
  defaultValue = JSON_NULL
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe TermType
toMaybe'Enum 0 = Prelude'.Just JSON_NULL
toMaybe'Enum 1 = Prelude'.Just VAR
toMaybe'Enum 2 = Prelude'.Just LET
toMaybe'Enum 3 = Prelude'.Just CALL
toMaybe'Enum 4 = Prelude'.Just IF
toMaybe'Enum 5 = Prelude'.Just ERROR
toMaybe'Enum 6 = Prelude'.Just NUMBER
toMaybe'Enum 7 = Prelude'.Just STRING
toMaybe'Enum 8 = Prelude'.Just JSON
toMaybe'Enum 9 = Prelude'.Just BOOL
toMaybe'Enum 10 = Prelude'.Just ARRAY
toMaybe'Enum 11 = Prelude'.Just OBJECT
toMaybe'Enum 12 = Prelude'.Just GETBYKEY
toMaybe'Enum 13 = Prelude'.Just TABLE
toMaybe'Enum 14 = Prelude'.Just JAVASCRIPT
toMaybe'Enum 15 = Prelude'.Just IMPLICIT_VAR
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum TermType where
  fromEnum JSON_NULL = 0
  fromEnum VAR = 1
  fromEnum LET = 2
  fromEnum CALL = 3
  fromEnum IF = 4
  fromEnum ERROR = 5
  fromEnum NUMBER = 6
  fromEnum STRING = 7
  fromEnum JSON = 8
  fromEnum BOOL = 9
  fromEnum ARRAY = 10
  fromEnum OBJECT = 11
  fromEnum GETBYKEY = 12
  fromEnum TABLE = 13
  fromEnum JAVASCRIPT = 14
  fromEnum IMPLICIT_VAR = 15
  toEnum
   = P'.fromMaybe
      (Prelude'.error "hprotoc generated code: toEnum failure for type Database.RethinkDB.Internal.Query_Language.Term.TermType")
      . toMaybe'Enum
  succ JSON_NULL = VAR
  succ VAR = LET
  succ LET = CALL
  succ CALL = IF
  succ IF = ERROR
  succ ERROR = NUMBER
  succ NUMBER = STRING
  succ STRING = JSON
  succ JSON = BOOL
  succ BOOL = ARRAY
  succ ARRAY = OBJECT
  succ OBJECT = GETBYKEY
  succ GETBYKEY = TABLE
  succ TABLE = JAVASCRIPT
  succ JAVASCRIPT = IMPLICIT_VAR
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Database.RethinkDB.Internal.Query_Language.Term.TermType"
  pred VAR = JSON_NULL
  pred LET = VAR
  pred CALL = LET
  pred IF = CALL
  pred ERROR = IF
  pred NUMBER = ERROR
  pred STRING = NUMBER
  pred JSON = STRING
  pred BOOL = JSON
  pred ARRAY = BOOL
  pred OBJECT = ARRAY
  pred GETBYKEY = OBJECT
  pred TABLE = GETBYKEY
  pred JAVASCRIPT = TABLE
  pred IMPLICIT_VAR = JAVASCRIPT
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Database.RethinkDB.Internal.Query_Language.Term.TermType"
 
instance P'.Wire TermType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB TermType
 
instance P'.MessageAPI msg' (msg' -> TermType) TermType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum TermType where
  reflectEnum
   = [(0, "JSON_NULL", JSON_NULL), (1, "VAR", VAR), (2, "LET", LET), (3, "CALL", CALL), (4, "IF", IF), (5, "ERROR", ERROR),
      (6, "NUMBER", NUMBER), (7, "STRING", STRING), (8, "JSON", JSON), (9, "BOOL", BOOL), (10, "ARRAY", ARRAY),
      (11, "OBJECT", OBJECT), (12, "GETBYKEY", GETBYKEY), (13, "TABLE", TABLE), (14, "JAVASCRIPT", JAVASCRIPT),
      (15, "IMPLICIT_VAR", IMPLICIT_VAR)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Query_Language.Term.TermType") ["Database", "RethinkDB", "Internal"] ["Query_Language", "Term"]
        "TermType")
      ["Database", "RethinkDB", "Internal", "Query_Language", "Term", "TermType.hs"]
      [(0, "JSON_NULL"), (1, "VAR"), (2, "LET"), (3, "CALL"), (4, "IF"), (5, "ERROR"), (6, "NUMBER"), (7, "STRING"), (8, "JSON"),
       (9, "BOOL"), (10, "ARRAY"), (11, "OBJECT"), (12, "GETBYKEY"), (13, "TABLE"), (14, "JAVASCRIPT"), (15, "IMPLICIT_VAR")]