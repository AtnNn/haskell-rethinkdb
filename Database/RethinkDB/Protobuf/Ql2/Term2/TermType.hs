{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Protobuf.Ql2.Term2.TermType (TermType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data TermType = DATUM
              | MAKE_ARRAY
              | MAKE_OBJ
              | VAR
              | JAVASCRIPT
              | ERROR
              | IMPLICIT_VAR
              | DB
              | TABLE
              | GET
              | EQ
              | NE
              | LT
              | LE
              | GT
              | GE
              | NOT
              | ADD
              | SUB
              | MUL
              | DIV
              | MOD
              | APPEND
              | SLICE
              | SKIP
              | LIMIT
              | GETATTR
              | CONTAINS
              | PLUCK
              | WITHOUT
              | MERGE
              | BETWEEN
              | REDUCE
              | MAP
              | FILTER
              | CONCATMAP
              | ORDERBY
              | DISTINCT
              | COUNT
              | UNION
              | NTH
              | GROUPED_MAP_REDUCE
              | GROUPBY
              | INNER_JOIN
              | OUTER_JOIN
              | EQ_JOIN
              | ZIP
              | COERCE_TO
              | TYPEOF
              | UPDATE
              | DELETE
              | REPLACE
              | INSERT
              | DB_CREATE
              | DB_DROP
              | DB_LIST
              | TABLE_CREATE
              | TABLE_DROP
              | TABLE_LIST
              | FUNCALL
              | BRANCH
              | ANY
              | ALL
              | FOREACH
              | FUNC
              | ASC
              | DESC
              deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable TermType
 
instance Prelude'.Bounded TermType where
  minBound = DATUM
  maxBound = DESC
 
instance P'.Default TermType where
  defaultValue = DATUM
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe TermType
toMaybe'Enum 1 = Prelude'.Just DATUM
toMaybe'Enum 2 = Prelude'.Just MAKE_ARRAY
toMaybe'Enum 3 = Prelude'.Just MAKE_OBJ
toMaybe'Enum 10 = Prelude'.Just VAR
toMaybe'Enum 11 = Prelude'.Just JAVASCRIPT
toMaybe'Enum 12 = Prelude'.Just ERROR
toMaybe'Enum 13 = Prelude'.Just IMPLICIT_VAR
toMaybe'Enum 14 = Prelude'.Just DB
toMaybe'Enum 15 = Prelude'.Just TABLE
toMaybe'Enum 16 = Prelude'.Just GET
toMaybe'Enum 17 = Prelude'.Just EQ
toMaybe'Enum 18 = Prelude'.Just NE
toMaybe'Enum 19 = Prelude'.Just LT
toMaybe'Enum 20 = Prelude'.Just LE
toMaybe'Enum 21 = Prelude'.Just GT
toMaybe'Enum 22 = Prelude'.Just GE
toMaybe'Enum 23 = Prelude'.Just NOT
toMaybe'Enum 24 = Prelude'.Just ADD
toMaybe'Enum 25 = Prelude'.Just SUB
toMaybe'Enum 26 = Prelude'.Just MUL
toMaybe'Enum 27 = Prelude'.Just DIV
toMaybe'Enum 28 = Prelude'.Just MOD
toMaybe'Enum 29 = Prelude'.Just APPEND
toMaybe'Enum 30 = Prelude'.Just SLICE
toMaybe'Enum 70 = Prelude'.Just SKIP
toMaybe'Enum 71 = Prelude'.Just LIMIT
toMaybe'Enum 31 = Prelude'.Just GETATTR
toMaybe'Enum 32 = Prelude'.Just CONTAINS
toMaybe'Enum 33 = Prelude'.Just PLUCK
toMaybe'Enum 34 = Prelude'.Just WITHOUT
toMaybe'Enum 35 = Prelude'.Just MERGE
toMaybe'Enum 36 = Prelude'.Just BETWEEN
toMaybe'Enum 37 = Prelude'.Just REDUCE
toMaybe'Enum 38 = Prelude'.Just MAP
toMaybe'Enum 39 = Prelude'.Just FILTER
toMaybe'Enum 40 = Prelude'.Just CONCATMAP
toMaybe'Enum 41 = Prelude'.Just ORDERBY
toMaybe'Enum 42 = Prelude'.Just DISTINCT
toMaybe'Enum 43 = Prelude'.Just COUNT
toMaybe'Enum 44 = Prelude'.Just UNION
toMaybe'Enum 45 = Prelude'.Just NTH
toMaybe'Enum 46 = Prelude'.Just GROUPED_MAP_REDUCE
toMaybe'Enum 47 = Prelude'.Just GROUPBY
toMaybe'Enum 48 = Prelude'.Just INNER_JOIN
toMaybe'Enum 49 = Prelude'.Just OUTER_JOIN
toMaybe'Enum 50 = Prelude'.Just EQ_JOIN
toMaybe'Enum 72 = Prelude'.Just ZIP
toMaybe'Enum 51 = Prelude'.Just COERCE_TO
toMaybe'Enum 52 = Prelude'.Just TYPEOF
toMaybe'Enum 53 = Prelude'.Just UPDATE
toMaybe'Enum 54 = Prelude'.Just DELETE
toMaybe'Enum 55 = Prelude'.Just REPLACE
toMaybe'Enum 56 = Prelude'.Just INSERT
toMaybe'Enum 57 = Prelude'.Just DB_CREATE
toMaybe'Enum 58 = Prelude'.Just DB_DROP
toMaybe'Enum 59 = Prelude'.Just DB_LIST
toMaybe'Enum 60 = Prelude'.Just TABLE_CREATE
toMaybe'Enum 61 = Prelude'.Just TABLE_DROP
toMaybe'Enum 62 = Prelude'.Just TABLE_LIST
toMaybe'Enum 64 = Prelude'.Just FUNCALL
toMaybe'Enum 65 = Prelude'.Just BRANCH
toMaybe'Enum 66 = Prelude'.Just ANY
toMaybe'Enum 67 = Prelude'.Just ALL
toMaybe'Enum 68 = Prelude'.Just FOREACH
toMaybe'Enum 69 = Prelude'.Just FUNC
toMaybe'Enum 73 = Prelude'.Just ASC
toMaybe'Enum 74 = Prelude'.Just DESC
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum TermType where
  fromEnum DATUM = 1
  fromEnum MAKE_ARRAY = 2
  fromEnum MAKE_OBJ = 3
  fromEnum VAR = 10
  fromEnum JAVASCRIPT = 11
  fromEnum ERROR = 12
  fromEnum IMPLICIT_VAR = 13
  fromEnum DB = 14
  fromEnum TABLE = 15
  fromEnum GET = 16
  fromEnum EQ = 17
  fromEnum NE = 18
  fromEnum LT = 19
  fromEnum LE = 20
  fromEnum GT = 21
  fromEnum GE = 22
  fromEnum NOT = 23
  fromEnum ADD = 24
  fromEnum SUB = 25
  fromEnum MUL = 26
  fromEnum DIV = 27
  fromEnum MOD = 28
  fromEnum APPEND = 29
  fromEnum SLICE = 30
  fromEnum SKIP = 70
  fromEnum LIMIT = 71
  fromEnum GETATTR = 31
  fromEnum CONTAINS = 32
  fromEnum PLUCK = 33
  fromEnum WITHOUT = 34
  fromEnum MERGE = 35
  fromEnum BETWEEN = 36
  fromEnum REDUCE = 37
  fromEnum MAP = 38
  fromEnum FILTER = 39
  fromEnum CONCATMAP = 40
  fromEnum ORDERBY = 41
  fromEnum DISTINCT = 42
  fromEnum COUNT = 43
  fromEnum UNION = 44
  fromEnum NTH = 45
  fromEnum GROUPED_MAP_REDUCE = 46
  fromEnum GROUPBY = 47
  fromEnum INNER_JOIN = 48
  fromEnum OUTER_JOIN = 49
  fromEnum EQ_JOIN = 50
  fromEnum ZIP = 72
  fromEnum COERCE_TO = 51
  fromEnum TYPEOF = 52
  fromEnum UPDATE = 53
  fromEnum DELETE = 54
  fromEnum REPLACE = 55
  fromEnum INSERT = 56
  fromEnum DB_CREATE = 57
  fromEnum DB_DROP = 58
  fromEnum DB_LIST = 59
  fromEnum TABLE_CREATE = 60
  fromEnum TABLE_DROP = 61
  fromEnum TABLE_LIST = 62
  fromEnum FUNCALL = 64
  fromEnum BRANCH = 65
  fromEnum ANY = 66
  fromEnum ALL = 67
  fromEnum FOREACH = 68
  fromEnum FUNC = 69
  fromEnum ASC = 73
  fromEnum DESC = 74
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Database.RethinkDB.Protobuf.Ql2.Term2.TermType")
      . toMaybe'Enum
  succ DATUM = MAKE_ARRAY
  succ MAKE_ARRAY = MAKE_OBJ
  succ MAKE_OBJ = VAR
  succ VAR = JAVASCRIPT
  succ JAVASCRIPT = ERROR
  succ ERROR = IMPLICIT_VAR
  succ IMPLICIT_VAR = DB
  succ DB = TABLE
  succ TABLE = GET
  succ GET = EQ
  succ EQ = NE
  succ NE = LT
  succ LT = LE
  succ LE = GT
  succ GT = GE
  succ GE = NOT
  succ NOT = ADD
  succ ADD = SUB
  succ SUB = MUL
  succ MUL = DIV
  succ DIV = MOD
  succ MOD = APPEND
  succ APPEND = SLICE
  succ SLICE = SKIP
  succ SKIP = LIMIT
  succ LIMIT = GETATTR
  succ GETATTR = CONTAINS
  succ CONTAINS = PLUCK
  succ PLUCK = WITHOUT
  succ WITHOUT = MERGE
  succ MERGE = BETWEEN
  succ BETWEEN = REDUCE
  succ REDUCE = MAP
  succ MAP = FILTER
  succ FILTER = CONCATMAP
  succ CONCATMAP = ORDERBY
  succ ORDERBY = DISTINCT
  succ DISTINCT = COUNT
  succ COUNT = UNION
  succ UNION = NTH
  succ NTH = GROUPED_MAP_REDUCE
  succ GROUPED_MAP_REDUCE = GROUPBY
  succ GROUPBY = INNER_JOIN
  succ INNER_JOIN = OUTER_JOIN
  succ OUTER_JOIN = EQ_JOIN
  succ EQ_JOIN = ZIP
  succ ZIP = COERCE_TO
  succ COERCE_TO = TYPEOF
  succ TYPEOF = UPDATE
  succ UPDATE = DELETE
  succ DELETE = REPLACE
  succ REPLACE = INSERT
  succ INSERT = DB_CREATE
  succ DB_CREATE = DB_DROP
  succ DB_DROP = DB_LIST
  succ DB_LIST = TABLE_CREATE
  succ TABLE_CREATE = TABLE_DROP
  succ TABLE_DROP = TABLE_LIST
  succ TABLE_LIST = FUNCALL
  succ FUNCALL = BRANCH
  succ BRANCH = ANY
  succ ANY = ALL
  succ ALL = FOREACH
  succ FOREACH = FUNC
  succ FUNC = ASC
  succ ASC = DESC
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Database.RethinkDB.Protobuf.Ql2.Term2.TermType"
  pred MAKE_ARRAY = DATUM
  pred MAKE_OBJ = MAKE_ARRAY
  pred VAR = MAKE_OBJ
  pred JAVASCRIPT = VAR
  pred ERROR = JAVASCRIPT
  pred IMPLICIT_VAR = ERROR
  pred DB = IMPLICIT_VAR
  pred TABLE = DB
  pred GET = TABLE
  pred EQ = GET
  pred NE = EQ
  pred LT = NE
  pred LE = LT
  pred GT = LE
  pred GE = GT
  pred NOT = GE
  pred ADD = NOT
  pred SUB = ADD
  pred MUL = SUB
  pred DIV = MUL
  pred MOD = DIV
  pred APPEND = MOD
  pred SLICE = APPEND
  pred SKIP = SLICE
  pred LIMIT = SKIP
  pred GETATTR = LIMIT
  pred CONTAINS = GETATTR
  pred PLUCK = CONTAINS
  pred WITHOUT = PLUCK
  pred MERGE = WITHOUT
  pred BETWEEN = MERGE
  pred REDUCE = BETWEEN
  pred MAP = REDUCE
  pred FILTER = MAP
  pred CONCATMAP = FILTER
  pred ORDERBY = CONCATMAP
  pred DISTINCT = ORDERBY
  pred COUNT = DISTINCT
  pred UNION = COUNT
  pred NTH = UNION
  pred GROUPED_MAP_REDUCE = NTH
  pred GROUPBY = GROUPED_MAP_REDUCE
  pred INNER_JOIN = GROUPBY
  pred OUTER_JOIN = INNER_JOIN
  pred EQ_JOIN = OUTER_JOIN
  pred ZIP = EQ_JOIN
  pred COERCE_TO = ZIP
  pred TYPEOF = COERCE_TO
  pred UPDATE = TYPEOF
  pred DELETE = UPDATE
  pred REPLACE = DELETE
  pred INSERT = REPLACE
  pred DB_CREATE = INSERT
  pred DB_DROP = DB_CREATE
  pred DB_LIST = DB_DROP
  pred TABLE_CREATE = DB_LIST
  pred TABLE_DROP = TABLE_CREATE
  pred TABLE_LIST = TABLE_DROP
  pred FUNCALL = TABLE_LIST
  pred BRANCH = FUNCALL
  pred ANY = BRANCH
  pred ALL = ANY
  pred FOREACH = ALL
  pred FUNC = FOREACH
  pred ASC = FUNC
  pred DESC = ASC
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Database.RethinkDB.Protobuf.Ql2.Term2.TermType"
 
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
   = [(1, "DATUM", DATUM), (2, "MAKE_ARRAY", MAKE_ARRAY), (3, "MAKE_OBJ", MAKE_OBJ), (10, "VAR", VAR),
      (11, "JAVASCRIPT", JAVASCRIPT), (12, "ERROR", ERROR), (13, "IMPLICIT_VAR", IMPLICIT_VAR), (14, "DB", DB),
      (15, "TABLE", TABLE), (16, "GET", GET), (17, "EQ", EQ), (18, "NE", NE), (19, "LT", LT), (20, "LE", LE), (21, "GT", GT),
      (22, "GE", GE), (23, "NOT", NOT), (24, "ADD", ADD), (25, "SUB", SUB), (26, "MUL", MUL), (27, "DIV", DIV), (28, "MOD", MOD),
      (29, "APPEND", APPEND), (30, "SLICE", SLICE), (70, "SKIP", SKIP), (71, "LIMIT", LIMIT), (31, "GETATTR", GETATTR),
      (32, "CONTAINS", CONTAINS), (33, "PLUCK", PLUCK), (34, "WITHOUT", WITHOUT), (35, "MERGE", MERGE), (36, "BETWEEN", BETWEEN),
      (37, "REDUCE", REDUCE), (38, "MAP", MAP), (39, "FILTER", FILTER), (40, "CONCATMAP", CONCATMAP), (41, "ORDERBY", ORDERBY),
      (42, "DISTINCT", DISTINCT), (43, "COUNT", COUNT), (44, "UNION", UNION), (45, "NTH", NTH),
      (46, "GROUPED_MAP_REDUCE", GROUPED_MAP_REDUCE), (47, "GROUPBY", GROUPBY), (48, "INNER_JOIN", INNER_JOIN),
      (49, "OUTER_JOIN", OUTER_JOIN), (50, "EQ_JOIN", EQ_JOIN), (72, "ZIP", ZIP), (51, "COERCE_TO", COERCE_TO),
      (52, "TYPEOF", TYPEOF), (53, "UPDATE", UPDATE), (54, "DELETE", DELETE), (55, "REPLACE", REPLACE), (56, "INSERT", INSERT),
      (57, "DB_CREATE", DB_CREATE), (58, "DB_DROP", DB_DROP), (59, "DB_LIST", DB_LIST), (60, "TABLE_CREATE", TABLE_CREATE),
      (61, "TABLE_DROP", TABLE_DROP), (62, "TABLE_LIST", TABLE_LIST), (64, "FUNCALL", FUNCALL), (65, "BRANCH", BRANCH),
      (66, "ANY", ANY), (67, "ALL", ALL), (68, "FOREACH", FOREACH), (69, "FUNC", FUNC), (73, "ASC", ASC), (74, "DESC", DESC)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Ql2.Term2.TermType") ["Database", "RethinkDB", "Protobuf"] ["Ql2", "Term2"] "TermType")
      ["Database", "RethinkDB", "Protobuf", "Ql2", "Term2", "TermType.hs"]
      [(1, "DATUM"), (2, "MAKE_ARRAY"), (3, "MAKE_OBJ"), (10, "VAR"), (11, "JAVASCRIPT"), (12, "ERROR"), (13, "IMPLICIT_VAR"),
       (14, "DB"), (15, "TABLE"), (16, "GET"), (17, "EQ"), (18, "NE"), (19, "LT"), (20, "LE"), (21, "GT"), (22, "GE"), (23, "NOT"),
       (24, "ADD"), (25, "SUB"), (26, "MUL"), (27, "DIV"), (28, "MOD"), (29, "APPEND"), (30, "SLICE"), (70, "SKIP"), (71, "LIMIT"),
       (31, "GETATTR"), (32, "CONTAINS"), (33, "PLUCK"), (34, "WITHOUT"), (35, "MERGE"), (36, "BETWEEN"), (37, "REDUCE"),
       (38, "MAP"), (39, "FILTER"), (40, "CONCATMAP"), (41, "ORDERBY"), (42, "DISTINCT"), (43, "COUNT"), (44, "UNION"), (45, "NTH"),
       (46, "GROUPED_MAP_REDUCE"), (47, "GROUPBY"), (48, "INNER_JOIN"), (49, "OUTER_JOIN"), (50, "EQ_JOIN"), (72, "ZIP"),
       (51, "COERCE_TO"), (52, "TYPEOF"), (53, "UPDATE"), (54, "DELETE"), (55, "REPLACE"), (56, "INSERT"), (57, "DB_CREATE"),
       (58, "DB_DROP"), (59, "DB_LIST"), (60, "TABLE_CREATE"), (61, "TABLE_DROP"), (62, "TABLE_LIST"), (64, "FUNCALL"),
       (65, "BRANCH"), (66, "ANY"), (67, "ALL"), (68, "FOREACH"), (69, "FUNC"), (73, "ASC"), (74, "DESC")]