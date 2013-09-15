{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

-- | Functions from the ReQL (RethinkDB Query Language)

module Database.RethinkDB.Functions where

import Data.Text (Text)
import Control.Monad.State
import Control.Applicative
import Data.Maybe

import Database.RethinkDB.ReQL
import Database.RethinkDB.MapReduce
import Database.RethinkDB.Objects as O

import Database.RethinkDB.Protobuf.Ql2.Term.TermType

import Prelude (($), return, Double, Bool, String)
import qualified Prelude as P

(+), (-), (*), (/), mod
  :: (Expr a, Expr b) => a -> b -> ReQL
(+) a b = op ADD (a, b) ()
(-) a b = op SUB (a, b) ()
(*) a b = op MUL (a, b) ()
(/) a b = op DIV (a, b) ()
mod a b = op MOD (a, b) ()

(||), (&&) :: (Expr a, Expr b) => a -> b -> ReQL
a || b = op ANY (a, b) ()
a && b = op ALL (a, b) ()

(==), (!=) :: (Expr a, Expr b) => a -> b -> ReQL
a == b = op EQ (a, b) ()
a != b = op NE (a, b) ()

(>), (>=), (<), (<=)
  :: (Expr a, Expr b) => a -> b -> ReQL
a > b = op GT (a, b) ()
a < b = op LT (a, b) ()
a >=b = op GE (a, b) ()
a <=b = op LE (a, b) ()

not :: (Expr a) => a -> ReQL
not a = op NOT [a] ()

-- * Lists and Streams

count :: (Expr a) => a -> ReQL
count e = op COUNT [e] ()

(++) :: (Expr a, Expr b) => a -> b -> ReQL
a ++ b = op UNION (a, b) ()

map :: (Expr a) => (ReQL -> ReQL) -> a -> ReQL
map f a = op MAP (a, f) ()

filter :: (Expr a) => (ReQL -> ReQL) -> a -> ReQL
filter f a = op FILTER (a, f) ()

between :: (Expr a, Expr b, Expr s) => a -> b -> s -> ReQL
between a b e = op BETWEEN [e] ["left_bound" := a, "right_bound" := b]

append :: (Expr a, Expr b) => a -> b -> ReQL
append a b = op APPEND (b, a) ()

concatMap :: (Expr a) => (ReQL -> ReQL) -> a -> ReQL
concatMap f e = op CONCATMAP (e, f) ()

innerJoin, outerJoin :: (Expr a, Expr b) => (ReQL -> ReQL) -> a -> b -> ReQL
innerJoin f a b = op INNER_JOIN (a, b, f) ()
outerJoin f a b = op OUTER_JOIN (a, b, f) ()

eqJoin :: (Expr a, Expr b) => a -> Key -> b -> ReQL
eqJoin a k b = op EQ_JOIN (a, expr k, b) ()

drop :: (Expr a, Expr b) => a -> b -> ReQL
drop a b = op SKIP (b, a) ()

take :: (Expr a, Expr b) => a -> b -> ReQL
take a b = op LIMIT (a, b) ()

slice :: (Expr a, Expr b, Expr c) => a -> b -> c -> ReQL
slice n m s = op SLICE (s, n, m) ()

(!!) :: (Expr a, Expr b) => a -> b -> ReQL
s !! n = op NTH (s, n) ()

reduce :: (Expr b, Expr s) => (ReQL -> ReQL -> ReQL) -> b -> s -> ReQL
reduce f b s = op REDUCE (f, s) ["base" := b]

reduce1 :: (Expr s) => (ReQL -> ReQL -> ReQL) -> s -> ReQL
reduce1 f s = op REDUCE (f, s) ()

distinct :: (Expr s) => s -> ReQL
distinct s = op DISTINCT [s] ()

forEach :: (Expr s) => s -> (ReQL -> ReQL) -> ReQL
forEach s f = op FOREACH (s, f) ()

mergeRightLeft :: (Expr a) => a -> ReQL
mergeRightLeft a = op ZIP [a] ()

data Order =
  Asc { orderAttr :: Key } |
  Desc { orderAttr :: Key }

orderBy :: (Expr s) => [Order] -> s -> ReQL
orderBy o s = ReQL $ do
  s' <- baseReQL (expr s)
  o' <- baseArray $ arr $ P.map buildOrder o
  return $ BaseReQL ORDERBY P.Nothing (s' : o') []
  where
    buildOrder (Asc k) = op ASC [k] ()
    buildOrder (Desc k) = op DESC [k] ()

groupBy :: (ReQL -> ReQL) -> (ReQL -> ReQL) -> ReQL
groupBy g mr = ReQL $ do
  (m, r, f) <- termToMapReduce mr
  baseReQL $ op MAP (op GROUPED_MAP_REDUCE (g, m, r) (), f) ()

sum :: (Expr s) => s -> ReQL
sum = reduce ((+) :: ReQL -> ReQL -> ReQL) (0 :: ReQL)

avg :: (Expr s) => s -> ReQL
avg s = sum s / count s

-- * Accessors

(!) :: (Expr s) => s -> Key -> ReQL
s ! k = op GET_FIELD (s, k) ()

pluck :: (Expr o) => [Key] -> o -> ReQL
pluck ks e = op PLUCK (cons e $ arr (P.map expr ks)) ()

without :: (Expr o) => [Key] -> o -> ReQL
without ks e = op WITHOUT (cons e $ arr (P.map expr ks)) ()

member :: (Expr o) => [Key] -> o -> ReQL
member ks o = op CONTAINS (cons o $ arr (P.map expr ks)) ()

merge :: (Expr a, Expr b) => a -> b -> ReQL
merge a b = op MERGE (a, b) ()

class Javascript r where
  js :: P.String -> r

instance Javascript ReQL where
  js s = op JAVASCRIPT [str s] ()

instance Javascript (ReQL -> ReQL) where
  js s x = op FUNCALL (op JAVASCRIPT [str s] (), x) ()

instance Javascript (ReQL -> ReQL -> ReQL) where
  js s x y = op FUNCALL (op JAVASCRIPT [str s] (), x, y) ()

if' :: (Expr a, Expr b, Expr c) => a -> b -> c -> ReQL
if' a b c = op BRANCH (a, b, c) ()

error :: (Expr s) => s -> ReQL
error m = op ERROR [m] ()

-- | Create a Database reference
db :: Text -> O.Database
db s = O.Database s

-- | Create a database on the server
dbCreate :: P.String -> ReQL
dbCreate db_name = op DB_CREATE [str db_name] ()

-- | Drop a database
dbDrop :: Database -> ReQL
dbDrop (O.Database name) = op DB_DROP [name] ()

-- | List the databases on the server
--
dbList :: ReQL
dbList = op DB_LIST () ()

-- | Create a simple table refence with no associated database
table :: Text -> Table
table n = O.Table Nothing n Nothing

-- | Create a table on the server
tableCreate :: Table -> TableCreateOptions -> ReQL
tableCreate (O.Table mdb table_name pkey) opts =
  withQuerySettings $ \QuerySettings{ queryDefaultDatabase = ddb } ->
    op TABLE_CREATE (fromMaybe ddb mdb, table_name) $ catMaybes [
      ("datacenter" :=) <$> tableDataCenter opts,
      ("cache_size" :=) <$> tableCacheSize opts,
      ("primary_key" :=) <$> pkey ]

-- | Drop a table
tableDrop :: Table -> ReQL
tableDrop (O.Table mdb table_name _) =
  withQuerySettings $ \QuerySettings{ queryDefaultDatabase = ddb } ->
    op TABLE_DROP (fromMaybe ddb mdb, table_name) ()

-- | List the tables in a database
tableList :: Database -> ReQL
tableList (O.Database name) = op DB_LIST [name] ()

get :: (Expr s, Expr k) => k -> s -> ReQL
get k e = op GET (e, k) ()

insert :: (Expr table, Expr object) => object -> table -> ReQL
insert a tb = op INSERT (tb, a) ()

upsert :: (Expr table, Expr object) => object -> table -> ReQL
upsert a tb = op INSERT (tb, a) ["upsert" := P.True]

update :: (Expr selection) => (ReQL -> ReQL) -> selection -> ReQL
update f s = op UPDATE (s, f) ()

replace :: (Expr selection) => (ReQL -> ReQL) -> selection -> ReQL
replace f s = op REPLACE (s, f) ()

delete :: (Expr selection) => selection -> ReQL
delete s = op DELETE [s] ()

coerceTo :: (Expr type') => type' -> ReQL -> ReQL
coerceTo t a = op COERCE_TO (a, t) ()