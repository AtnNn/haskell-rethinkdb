{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Protobuf.Ql2.Term.TermType (TermType(..)) where
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
              | GET_ALL
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
              | PREPEND
              | DIFFERENCE
              | SET_INSERT
              | SET_INTERSECTION
              | SET_UNION
              | SET_DIFFERENCE
              | SLICE
              | SKIP
              | LIMIT
              | INDEXES_OF
              | CONTAINS
              | GET_FIELD
              | KEYS
              | OBJECT
              | HAS_FIELDS
              | WITH_FIELDS
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
              | IS_EMPTY
              | UNION
              | NTH
              | INNER_JOIN
              | OUTER_JOIN
              | EQ_JOIN
              | ZIP
              | INSERT_AT
              | DELETE_AT
              | CHANGE_AT
              | SPLICE_AT
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
              | SYNC
              | INDEX_CREATE
              | INDEX_DROP
              | INDEX_LIST
              | INDEX_STATUS
              | INDEX_WAIT
              | FUNCALL
              | BRANCH
              | ANY
              | ALL
              | FOREACH
              | FUNC
              | ASC
              | DESC
              | INFO
              | MATCH
              | UPCASE
              | DOWNCASE
              | SAMPLE
              | DEFAULT
              | JSON
              | ISO8601
              | TO_ISO8601
              | EPOCH_TIME
              | TO_EPOCH_TIME
              | NOW
              | IN_TIMEZONE
              | DURING
              | DATE
              | TIME_OF_DAY
              | TIMEZONE
              | YEAR
              | MONTH
              | DAY
              | DAY_OF_WEEK
              | DAY_OF_YEAR
              | HOURS
              | MINUTES
              | SECONDS
              | TIME
              | MONDAY
              | TUESDAY
              | WEDNESDAY
              | THURSDAY
              | FRIDAY
              | SATURDAY
              | SUNDAY
              | JANUARY
              | FEBRUARY
              | MARCH
              | APRIL
              | MAY
              | JUNE
              | JULY
              | AUGUST
              | SEPTEMBER
              | OCTOBER
              | NOVEMBER
              | DECEMBER
              | LITERAL
              | GROUP
              | SUM
              | AVG
              | MIN
              | MAX
              | SPLIT
              | UNGROUP
              deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable TermType
 
instance Prelude'.Bounded TermType where
  minBound = DATUM
  maxBound = UNGROUP
 
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
toMaybe'Enum 78 = Prelude'.Just GET_ALL
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
toMaybe'Enum 80 = Prelude'.Just PREPEND
toMaybe'Enum 95 = Prelude'.Just DIFFERENCE
toMaybe'Enum 88 = Prelude'.Just SET_INSERT
toMaybe'Enum 89 = Prelude'.Just SET_INTERSECTION
toMaybe'Enum 90 = Prelude'.Just SET_UNION
toMaybe'Enum 91 = Prelude'.Just SET_DIFFERENCE
toMaybe'Enum 30 = Prelude'.Just SLICE
toMaybe'Enum 70 = Prelude'.Just SKIP
toMaybe'Enum 71 = Prelude'.Just LIMIT
toMaybe'Enum 87 = Prelude'.Just INDEXES_OF
toMaybe'Enum 93 = Prelude'.Just CONTAINS
toMaybe'Enum 31 = Prelude'.Just GET_FIELD
toMaybe'Enum 94 = Prelude'.Just KEYS
toMaybe'Enum 143 = Prelude'.Just OBJECT
toMaybe'Enum 32 = Prelude'.Just HAS_FIELDS
toMaybe'Enum 96 = Prelude'.Just WITH_FIELDS
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
toMaybe'Enum 86 = Prelude'.Just IS_EMPTY
toMaybe'Enum 44 = Prelude'.Just UNION
toMaybe'Enum 45 = Prelude'.Just NTH
toMaybe'Enum 48 = Prelude'.Just INNER_JOIN
toMaybe'Enum 49 = Prelude'.Just OUTER_JOIN
toMaybe'Enum 50 = Prelude'.Just EQ_JOIN
toMaybe'Enum 72 = Prelude'.Just ZIP
toMaybe'Enum 82 = Prelude'.Just INSERT_AT
toMaybe'Enum 83 = Prelude'.Just DELETE_AT
toMaybe'Enum 84 = Prelude'.Just CHANGE_AT
toMaybe'Enum 85 = Prelude'.Just SPLICE_AT
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
toMaybe'Enum 138 = Prelude'.Just SYNC
toMaybe'Enum 75 = Prelude'.Just INDEX_CREATE
toMaybe'Enum 76 = Prelude'.Just INDEX_DROP
toMaybe'Enum 77 = Prelude'.Just INDEX_LIST
toMaybe'Enum 139 = Prelude'.Just INDEX_STATUS
toMaybe'Enum 140 = Prelude'.Just INDEX_WAIT
toMaybe'Enum 64 = Prelude'.Just FUNCALL
toMaybe'Enum 65 = Prelude'.Just BRANCH
toMaybe'Enum 66 = Prelude'.Just ANY
toMaybe'Enum 67 = Prelude'.Just ALL
toMaybe'Enum 68 = Prelude'.Just FOREACH
toMaybe'Enum 69 = Prelude'.Just FUNC
toMaybe'Enum 73 = Prelude'.Just ASC
toMaybe'Enum 74 = Prelude'.Just DESC
toMaybe'Enum 79 = Prelude'.Just INFO
toMaybe'Enum 97 = Prelude'.Just MATCH
toMaybe'Enum 141 = Prelude'.Just UPCASE
toMaybe'Enum 142 = Prelude'.Just DOWNCASE
toMaybe'Enum 81 = Prelude'.Just SAMPLE
toMaybe'Enum 92 = Prelude'.Just DEFAULT
toMaybe'Enum 98 = Prelude'.Just JSON
toMaybe'Enum 99 = Prelude'.Just ISO8601
toMaybe'Enum 100 = Prelude'.Just TO_ISO8601
toMaybe'Enum 101 = Prelude'.Just EPOCH_TIME
toMaybe'Enum 102 = Prelude'.Just TO_EPOCH_TIME
toMaybe'Enum 103 = Prelude'.Just NOW
toMaybe'Enum 104 = Prelude'.Just IN_TIMEZONE
toMaybe'Enum 105 = Prelude'.Just DURING
toMaybe'Enum 106 = Prelude'.Just DATE
toMaybe'Enum 126 = Prelude'.Just TIME_OF_DAY
toMaybe'Enum 127 = Prelude'.Just TIMEZONE
toMaybe'Enum 128 = Prelude'.Just YEAR
toMaybe'Enum 129 = Prelude'.Just MONTH
toMaybe'Enum 130 = Prelude'.Just DAY
toMaybe'Enum 131 = Prelude'.Just DAY_OF_WEEK
toMaybe'Enum 132 = Prelude'.Just DAY_OF_YEAR
toMaybe'Enum 133 = Prelude'.Just HOURS
toMaybe'Enum 134 = Prelude'.Just MINUTES
toMaybe'Enum 135 = Prelude'.Just SECONDS
toMaybe'Enum 136 = Prelude'.Just TIME
toMaybe'Enum 107 = Prelude'.Just MONDAY
toMaybe'Enum 108 = Prelude'.Just TUESDAY
toMaybe'Enum 109 = Prelude'.Just WEDNESDAY
toMaybe'Enum 110 = Prelude'.Just THURSDAY
toMaybe'Enum 111 = Prelude'.Just FRIDAY
toMaybe'Enum 112 = Prelude'.Just SATURDAY
toMaybe'Enum 113 = Prelude'.Just SUNDAY
toMaybe'Enum 114 = Prelude'.Just JANUARY
toMaybe'Enum 115 = Prelude'.Just FEBRUARY
toMaybe'Enum 116 = Prelude'.Just MARCH
toMaybe'Enum 117 = Prelude'.Just APRIL
toMaybe'Enum 118 = Prelude'.Just MAY
toMaybe'Enum 119 = Prelude'.Just JUNE
toMaybe'Enum 120 = Prelude'.Just JULY
toMaybe'Enum 121 = Prelude'.Just AUGUST
toMaybe'Enum 122 = Prelude'.Just SEPTEMBER
toMaybe'Enum 123 = Prelude'.Just OCTOBER
toMaybe'Enum 124 = Prelude'.Just NOVEMBER
toMaybe'Enum 125 = Prelude'.Just DECEMBER
toMaybe'Enum 137 = Prelude'.Just LITERAL
toMaybe'Enum 144 = Prelude'.Just GROUP
toMaybe'Enum 145 = Prelude'.Just SUM
toMaybe'Enum 146 = Prelude'.Just AVG
toMaybe'Enum 147 = Prelude'.Just MIN
toMaybe'Enum 148 = Prelude'.Just MAX
toMaybe'Enum 149 = Prelude'.Just SPLIT
toMaybe'Enum 150 = Prelude'.Just UNGROUP
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
  fromEnum GET_ALL = 78
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
  fromEnum PREPEND = 80
  fromEnum DIFFERENCE = 95
  fromEnum SET_INSERT = 88
  fromEnum SET_INTERSECTION = 89
  fromEnum SET_UNION = 90
  fromEnum SET_DIFFERENCE = 91
  fromEnum SLICE = 30
  fromEnum SKIP = 70
  fromEnum LIMIT = 71
  fromEnum INDEXES_OF = 87
  fromEnum CONTAINS = 93
  fromEnum GET_FIELD = 31
  fromEnum KEYS = 94
  fromEnum OBJECT = 143
  fromEnum HAS_FIELDS = 32
  fromEnum WITH_FIELDS = 96
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
  fromEnum IS_EMPTY = 86
  fromEnum UNION = 44
  fromEnum NTH = 45
  fromEnum INNER_JOIN = 48
  fromEnum OUTER_JOIN = 49
  fromEnum EQ_JOIN = 50
  fromEnum ZIP = 72
  fromEnum INSERT_AT = 82
  fromEnum DELETE_AT = 83
  fromEnum CHANGE_AT = 84
  fromEnum SPLICE_AT = 85
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
  fromEnum SYNC = 138
  fromEnum INDEX_CREATE = 75
  fromEnum INDEX_DROP = 76
  fromEnum INDEX_LIST = 77
  fromEnum INDEX_STATUS = 139
  fromEnum INDEX_WAIT = 140
  fromEnum FUNCALL = 64
  fromEnum BRANCH = 65
  fromEnum ANY = 66
  fromEnum ALL = 67
  fromEnum FOREACH = 68
  fromEnum FUNC = 69
  fromEnum ASC = 73
  fromEnum DESC = 74
  fromEnum INFO = 79
  fromEnum MATCH = 97
  fromEnum UPCASE = 141
  fromEnum DOWNCASE = 142
  fromEnum SAMPLE = 81
  fromEnum DEFAULT = 92
  fromEnum JSON = 98
  fromEnum ISO8601 = 99
  fromEnum TO_ISO8601 = 100
  fromEnum EPOCH_TIME = 101
  fromEnum TO_EPOCH_TIME = 102
  fromEnum NOW = 103
  fromEnum IN_TIMEZONE = 104
  fromEnum DURING = 105
  fromEnum DATE = 106
  fromEnum TIME_OF_DAY = 126
  fromEnum TIMEZONE = 127
  fromEnum YEAR = 128
  fromEnum MONTH = 129
  fromEnum DAY = 130
  fromEnum DAY_OF_WEEK = 131
  fromEnum DAY_OF_YEAR = 132
  fromEnum HOURS = 133
  fromEnum MINUTES = 134
  fromEnum SECONDS = 135
  fromEnum TIME = 136
  fromEnum MONDAY = 107
  fromEnum TUESDAY = 108
  fromEnum WEDNESDAY = 109
  fromEnum THURSDAY = 110
  fromEnum FRIDAY = 111
  fromEnum SATURDAY = 112
  fromEnum SUNDAY = 113
  fromEnum JANUARY = 114
  fromEnum FEBRUARY = 115
  fromEnum MARCH = 116
  fromEnum APRIL = 117
  fromEnum MAY = 118
  fromEnum JUNE = 119
  fromEnum JULY = 120
  fromEnum AUGUST = 121
  fromEnum SEPTEMBER = 122
  fromEnum OCTOBER = 123
  fromEnum NOVEMBER = 124
  fromEnum DECEMBER = 125
  fromEnum LITERAL = 137
  fromEnum GROUP = 144
  fromEnum SUM = 145
  fromEnum AVG = 146
  fromEnum MIN = 147
  fromEnum MAX = 148
  fromEnum SPLIT = 149
  fromEnum UNGROUP = 150
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Database.RethinkDB.Protobuf.Ql2.Term.TermType") .
      toMaybe'Enum
  succ DATUM = MAKE_ARRAY
  succ MAKE_ARRAY = MAKE_OBJ
  succ MAKE_OBJ = VAR
  succ VAR = JAVASCRIPT
  succ JAVASCRIPT = ERROR
  succ ERROR = IMPLICIT_VAR
  succ IMPLICIT_VAR = DB
  succ DB = TABLE
  succ TABLE = GET
  succ GET = GET_ALL
  succ GET_ALL = EQ
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
  succ APPEND = PREPEND
  succ PREPEND = DIFFERENCE
  succ DIFFERENCE = SET_INSERT
  succ SET_INSERT = SET_INTERSECTION
  succ SET_INTERSECTION = SET_UNION
  succ SET_UNION = SET_DIFFERENCE
  succ SET_DIFFERENCE = SLICE
  succ SLICE = SKIP
  succ SKIP = LIMIT
  succ LIMIT = INDEXES_OF
  succ INDEXES_OF = CONTAINS
  succ CONTAINS = GET_FIELD
  succ GET_FIELD = KEYS
  succ KEYS = OBJECT
  succ OBJECT = HAS_FIELDS
  succ HAS_FIELDS = WITH_FIELDS
  succ WITH_FIELDS = PLUCK
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
  succ COUNT = IS_EMPTY
  succ IS_EMPTY = UNION
  succ UNION = NTH
  succ NTH = INNER_JOIN
  succ INNER_JOIN = OUTER_JOIN
  succ OUTER_JOIN = EQ_JOIN
  succ EQ_JOIN = ZIP
  succ ZIP = INSERT_AT
  succ INSERT_AT = DELETE_AT
  succ DELETE_AT = CHANGE_AT
  succ CHANGE_AT = SPLICE_AT
  succ SPLICE_AT = COERCE_TO
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
  succ TABLE_LIST = SYNC
  succ SYNC = INDEX_CREATE
  succ INDEX_CREATE = INDEX_DROP
  succ INDEX_DROP = INDEX_LIST
  succ INDEX_LIST = INDEX_STATUS
  succ INDEX_STATUS = INDEX_WAIT
  succ INDEX_WAIT = FUNCALL
  succ FUNCALL = BRANCH
  succ BRANCH = ANY
  succ ANY = ALL
  succ ALL = FOREACH
  succ FOREACH = FUNC
  succ FUNC = ASC
  succ ASC = DESC
  succ DESC = INFO
  succ INFO = MATCH
  succ MATCH = UPCASE
  succ UPCASE = DOWNCASE
  succ DOWNCASE = SAMPLE
  succ SAMPLE = DEFAULT
  succ DEFAULT = JSON
  succ JSON = ISO8601
  succ ISO8601 = TO_ISO8601
  succ TO_ISO8601 = EPOCH_TIME
  succ EPOCH_TIME = TO_EPOCH_TIME
  succ TO_EPOCH_TIME = NOW
  succ NOW = IN_TIMEZONE
  succ IN_TIMEZONE = DURING
  succ DURING = DATE
  succ DATE = TIME_OF_DAY
  succ TIME_OF_DAY = TIMEZONE
  succ TIMEZONE = YEAR
  succ YEAR = MONTH
  succ MONTH = DAY
  succ DAY = DAY_OF_WEEK
  succ DAY_OF_WEEK = DAY_OF_YEAR
  succ DAY_OF_YEAR = HOURS
  succ HOURS = MINUTES
  succ MINUTES = SECONDS
  succ SECONDS = TIME
  succ TIME = MONDAY
  succ MONDAY = TUESDAY
  succ TUESDAY = WEDNESDAY
  succ WEDNESDAY = THURSDAY
  succ THURSDAY = FRIDAY
  succ FRIDAY = SATURDAY
  succ SATURDAY = SUNDAY
  succ SUNDAY = JANUARY
  succ JANUARY = FEBRUARY
  succ FEBRUARY = MARCH
  succ MARCH = APRIL
  succ APRIL = MAY
  succ MAY = JUNE
  succ JUNE = JULY
  succ JULY = AUGUST
  succ AUGUST = SEPTEMBER
  succ SEPTEMBER = OCTOBER
  succ OCTOBER = NOVEMBER
  succ NOVEMBER = DECEMBER
  succ DECEMBER = LITERAL
  succ LITERAL = GROUP
  succ GROUP = SUM
  succ SUM = AVG
  succ AVG = MIN
  succ MIN = MAX
  succ MAX = SPLIT
  succ SPLIT = UNGROUP
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Database.RethinkDB.Protobuf.Ql2.Term.TermType"
  pred MAKE_ARRAY = DATUM
  pred MAKE_OBJ = MAKE_ARRAY
  pred VAR = MAKE_OBJ
  pred JAVASCRIPT = VAR
  pred ERROR = JAVASCRIPT
  pred IMPLICIT_VAR = ERROR
  pred DB = IMPLICIT_VAR
  pred TABLE = DB
  pred GET = TABLE
  pred GET_ALL = GET
  pred EQ = GET_ALL
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
  pred PREPEND = APPEND
  pred DIFFERENCE = PREPEND
  pred SET_INSERT = DIFFERENCE
  pred SET_INTERSECTION = SET_INSERT
  pred SET_UNION = SET_INTERSECTION
  pred SET_DIFFERENCE = SET_UNION
  pred SLICE = SET_DIFFERENCE
  pred SKIP = SLICE
  pred LIMIT = SKIP
  pred INDEXES_OF = LIMIT
  pred CONTAINS = INDEXES_OF
  pred GET_FIELD = CONTAINS
  pred KEYS = GET_FIELD
  pred OBJECT = KEYS
  pred HAS_FIELDS = OBJECT
  pred WITH_FIELDS = HAS_FIELDS
  pred PLUCK = WITH_FIELDS
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
  pred IS_EMPTY = COUNT
  pred UNION = IS_EMPTY
  pred NTH = UNION
  pred INNER_JOIN = NTH
  pred OUTER_JOIN = INNER_JOIN
  pred EQ_JOIN = OUTER_JOIN
  pred ZIP = EQ_JOIN
  pred INSERT_AT = ZIP
  pred DELETE_AT = INSERT_AT
  pred CHANGE_AT = DELETE_AT
  pred SPLICE_AT = CHANGE_AT
  pred COERCE_TO = SPLICE_AT
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
  pred SYNC = TABLE_LIST
  pred INDEX_CREATE = SYNC
  pred INDEX_DROP = INDEX_CREATE
  pred INDEX_LIST = INDEX_DROP
  pred INDEX_STATUS = INDEX_LIST
  pred INDEX_WAIT = INDEX_STATUS
  pred FUNCALL = INDEX_WAIT
  pred BRANCH = FUNCALL
  pred ANY = BRANCH
  pred ALL = ANY
  pred FOREACH = ALL
  pred FUNC = FOREACH
  pred ASC = FUNC
  pred DESC = ASC
  pred INFO = DESC
  pred MATCH = INFO
  pred UPCASE = MATCH
  pred DOWNCASE = UPCASE
  pred SAMPLE = DOWNCASE
  pred DEFAULT = SAMPLE
  pred JSON = DEFAULT
  pred ISO8601 = JSON
  pred TO_ISO8601 = ISO8601
  pred EPOCH_TIME = TO_ISO8601
  pred TO_EPOCH_TIME = EPOCH_TIME
  pred NOW = TO_EPOCH_TIME
  pred IN_TIMEZONE = NOW
  pred DURING = IN_TIMEZONE
  pred DATE = DURING
  pred TIME_OF_DAY = DATE
  pred TIMEZONE = TIME_OF_DAY
  pred YEAR = TIMEZONE
  pred MONTH = YEAR
  pred DAY = MONTH
  pred DAY_OF_WEEK = DAY
  pred DAY_OF_YEAR = DAY_OF_WEEK
  pred HOURS = DAY_OF_YEAR
  pred MINUTES = HOURS
  pred SECONDS = MINUTES
  pred TIME = SECONDS
  pred MONDAY = TIME
  pred TUESDAY = MONDAY
  pred WEDNESDAY = TUESDAY
  pred THURSDAY = WEDNESDAY
  pred FRIDAY = THURSDAY
  pred SATURDAY = FRIDAY
  pred SUNDAY = SATURDAY
  pred JANUARY = SUNDAY
  pred FEBRUARY = JANUARY
  pred MARCH = FEBRUARY
  pred APRIL = MARCH
  pred MAY = APRIL
  pred JUNE = MAY
  pred JULY = JUNE
  pred AUGUST = JULY
  pred SEPTEMBER = AUGUST
  pred OCTOBER = SEPTEMBER
  pred NOVEMBER = OCTOBER
  pred DECEMBER = NOVEMBER
  pred LITERAL = DECEMBER
  pred GROUP = LITERAL
  pred SUM = GROUP
  pred AVG = SUM
  pred MIN = AVG
  pred MAX = MIN
  pred SPLIT = MAX
  pred UNGROUP = SPLIT
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Database.RethinkDB.Protobuf.Ql2.Term.TermType"
 
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
      (15, "TABLE", TABLE), (16, "GET", GET), (78, "GET_ALL", GET_ALL), (17, "EQ", EQ), (18, "NE", NE), (19, "LT", LT),
      (20, "LE", LE), (21, "GT", GT), (22, "GE", GE), (23, "NOT", NOT), (24, "ADD", ADD), (25, "SUB", SUB), (26, "MUL", MUL),
      (27, "DIV", DIV), (28, "MOD", MOD), (29, "APPEND", APPEND), (80, "PREPEND", PREPEND), (95, "DIFFERENCE", DIFFERENCE),
      (88, "SET_INSERT", SET_INSERT), (89, "SET_INTERSECTION", SET_INTERSECTION), (90, "SET_UNION", SET_UNION),
      (91, "SET_DIFFERENCE", SET_DIFFERENCE), (30, "SLICE", SLICE), (70, "SKIP", SKIP), (71, "LIMIT", LIMIT),
      (87, "INDEXES_OF", INDEXES_OF), (93, "CONTAINS", CONTAINS), (31, "GET_FIELD", GET_FIELD), (94, "KEYS", KEYS),
      (143, "OBJECT", OBJECT), (32, "HAS_FIELDS", HAS_FIELDS), (96, "WITH_FIELDS", WITH_FIELDS), (33, "PLUCK", PLUCK),
      (34, "WITHOUT", WITHOUT), (35, "MERGE", MERGE), (36, "BETWEEN", BETWEEN), (37, "REDUCE", REDUCE), (38, "MAP", MAP),
      (39, "FILTER", FILTER), (40, "CONCATMAP", CONCATMAP), (41, "ORDERBY", ORDERBY), (42, "DISTINCT", DISTINCT),
      (43, "COUNT", COUNT), (86, "IS_EMPTY", IS_EMPTY), (44, "UNION", UNION), (45, "NTH", NTH), (48, "INNER_JOIN", INNER_JOIN),
      (49, "OUTER_JOIN", OUTER_JOIN), (50, "EQ_JOIN", EQ_JOIN), (72, "ZIP", ZIP), (82, "INSERT_AT", INSERT_AT),
      (83, "DELETE_AT", DELETE_AT), (84, "CHANGE_AT", CHANGE_AT), (85, "SPLICE_AT", SPLICE_AT), (51, "COERCE_TO", COERCE_TO),
      (52, "TYPEOF", TYPEOF), (53, "UPDATE", UPDATE), (54, "DELETE", DELETE), (55, "REPLACE", REPLACE), (56, "INSERT", INSERT),
      (57, "DB_CREATE", DB_CREATE), (58, "DB_DROP", DB_DROP), (59, "DB_LIST", DB_LIST), (60, "TABLE_CREATE", TABLE_CREATE),
      (61, "TABLE_DROP", TABLE_DROP), (62, "TABLE_LIST", TABLE_LIST), (138, "SYNC", SYNC), (75, "INDEX_CREATE", INDEX_CREATE),
      (76, "INDEX_DROP", INDEX_DROP), (77, "INDEX_LIST", INDEX_LIST), (139, "INDEX_STATUS", INDEX_STATUS),
      (140, "INDEX_WAIT", INDEX_WAIT), (64, "FUNCALL", FUNCALL), (65, "BRANCH", BRANCH), (66, "ANY", ANY), (67, "ALL", ALL),
      (68, "FOREACH", FOREACH), (69, "FUNC", FUNC), (73, "ASC", ASC), (74, "DESC", DESC), (79, "INFO", INFO), (97, "MATCH", MATCH),
      (141, "UPCASE", UPCASE), (142, "DOWNCASE", DOWNCASE), (81, "SAMPLE", SAMPLE), (92, "DEFAULT", DEFAULT), (98, "JSON", JSON),
      (99, "ISO8601", ISO8601), (100, "TO_ISO8601", TO_ISO8601), (101, "EPOCH_TIME", EPOCH_TIME),
      (102, "TO_EPOCH_TIME", TO_EPOCH_TIME), (103, "NOW", NOW), (104, "IN_TIMEZONE", IN_TIMEZONE), (105, "DURING", DURING),
      (106, "DATE", DATE), (126, "TIME_OF_DAY", TIME_OF_DAY), (127, "TIMEZONE", TIMEZONE), (128, "YEAR", YEAR),
      (129, "MONTH", MONTH), (130, "DAY", DAY), (131, "DAY_OF_WEEK", DAY_OF_WEEK), (132, "DAY_OF_YEAR", DAY_OF_YEAR),
      (133, "HOURS", HOURS), (134, "MINUTES", MINUTES), (135, "SECONDS", SECONDS), (136, "TIME", TIME), (107, "MONDAY", MONDAY),
      (108, "TUESDAY", TUESDAY), (109, "WEDNESDAY", WEDNESDAY), (110, "THURSDAY", THURSDAY), (111, "FRIDAY", FRIDAY),
      (112, "SATURDAY", SATURDAY), (113, "SUNDAY", SUNDAY), (114, "JANUARY", JANUARY), (115, "FEBRUARY", FEBRUARY),
      (116, "MARCH", MARCH), (117, "APRIL", APRIL), (118, "MAY", MAY), (119, "JUNE", JUNE), (120, "JULY", JULY),
      (121, "AUGUST", AUGUST), (122, "SEPTEMBER", SEPTEMBER), (123, "OCTOBER", OCTOBER), (124, "NOVEMBER", NOVEMBER),
      (125, "DECEMBER", DECEMBER), (137, "LITERAL", LITERAL), (144, "GROUP", GROUP), (145, "SUM", SUM), (146, "AVG", AVG),
      (147, "MIN", MIN), (148, "MAX", MAX), (149, "SPLIT", SPLIT), (150, "UNGROUP", UNGROUP)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Ql2.Term.TermType") ["Database", "RethinkDB", "Protobuf"] ["Ql2", "Term"] "TermType")
      ["Database", "RethinkDB", "Protobuf", "Ql2", "Term", "TermType.hs"]
      [(1, "DATUM"), (2, "MAKE_ARRAY"), (3, "MAKE_OBJ"), (10, "VAR"), (11, "JAVASCRIPT"), (12, "ERROR"), (13, "IMPLICIT_VAR"),
       (14, "DB"), (15, "TABLE"), (16, "GET"), (78, "GET_ALL"), (17, "EQ"), (18, "NE"), (19, "LT"), (20, "LE"), (21, "GT"),
       (22, "GE"), (23, "NOT"), (24, "ADD"), (25, "SUB"), (26, "MUL"), (27, "DIV"), (28, "MOD"), (29, "APPEND"), (80, "PREPEND"),
       (95, "DIFFERENCE"), (88, "SET_INSERT"), (89, "SET_INTERSECTION"), (90, "SET_UNION"), (91, "SET_DIFFERENCE"), (30, "SLICE"),
       (70, "SKIP"), (71, "LIMIT"), (87, "INDEXES_OF"), (93, "CONTAINS"), (31, "GET_FIELD"), (94, "KEYS"), (143, "OBJECT"),
       (32, "HAS_FIELDS"), (96, "WITH_FIELDS"), (33, "PLUCK"), (34, "WITHOUT"), (35, "MERGE"), (36, "BETWEEN"), (37, "REDUCE"),
       (38, "MAP"), (39, "FILTER"), (40, "CONCATMAP"), (41, "ORDERBY"), (42, "DISTINCT"), (43, "COUNT"), (86, "IS_EMPTY"),
       (44, "UNION"), (45, "NTH"), (48, "INNER_JOIN"), (49, "OUTER_JOIN"), (50, "EQ_JOIN"), (72, "ZIP"), (82, "INSERT_AT"),
       (83, "DELETE_AT"), (84, "CHANGE_AT"), (85, "SPLICE_AT"), (51, "COERCE_TO"), (52, "TYPEOF"), (53, "UPDATE"), (54, "DELETE"),
       (55, "REPLACE"), (56, "INSERT"), (57, "DB_CREATE"), (58, "DB_DROP"), (59, "DB_LIST"), (60, "TABLE_CREATE"),
       (61, "TABLE_DROP"), (62, "TABLE_LIST"), (138, "SYNC"), (75, "INDEX_CREATE"), (76, "INDEX_DROP"), (77, "INDEX_LIST"),
       (139, "INDEX_STATUS"), (140, "INDEX_WAIT"), (64, "FUNCALL"), (65, "BRANCH"), (66, "ANY"), (67, "ALL"), (68, "FOREACH"),
       (69, "FUNC"), (73, "ASC"), (74, "DESC"), (79, "INFO"), (97, "MATCH"), (141, "UPCASE"), (142, "DOWNCASE"), (81, "SAMPLE"),
       (92, "DEFAULT"), (98, "JSON"), (99, "ISO8601"), (100, "TO_ISO8601"), (101, "EPOCH_TIME"), (102, "TO_EPOCH_TIME"),
       (103, "NOW"), (104, "IN_TIMEZONE"), (105, "DURING"), (106, "DATE"), (126, "TIME_OF_DAY"), (127, "TIMEZONE"), (128, "YEAR"),
       (129, "MONTH"), (130, "DAY"), (131, "DAY_OF_WEEK"), (132, "DAY_OF_YEAR"), (133, "HOURS"), (134, "MINUTES"), (135, "SECONDS"),
       (136, "TIME"), (107, "MONDAY"), (108, "TUESDAY"), (109, "WEDNESDAY"), (110, "THURSDAY"), (111, "FRIDAY"), (112, "SATURDAY"),
       (113, "SUNDAY"), (114, "JANUARY"), (115, "FEBRUARY"), (116, "MARCH"), (117, "APRIL"), (118, "MAY"), (119, "JUNE"),
       (120, "JULY"), (121, "AUGUST"), (122, "SEPTEMBER"), (123, "OCTOBER"), (124, "NOVEMBER"), (125, "DECEMBER"), (137, "LITERAL"),
       (144, "GROUP"), (145, "SUM"), (146, "AVG"), (147, "MIN"), (148, "MAX"), (149, "SPLIT"), (150, "UNGROUP")]