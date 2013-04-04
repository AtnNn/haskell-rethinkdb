{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds, FlexibleContexts,
             PolyKinds, ScopedTypeVariables,
             OverloadedStrings, FlexibleInstances #-}

-- | Functions from the ReQL (RethinkDB Query Language)

module Database.RethinkDB.Functions where

import Data.Text (Text)
import Control.Monad.State
import Control.Applicative
import Data.Maybe

import Database.RethinkDB.Term
import Database.RethinkDB.MapReduce
import Database.RethinkDB.Objects as O

import Database.RethinkDB.Protobuf.Ql2.Term2.TermType

import Prelude (($), return, Double, Bool, String)
import qualified Prelude as P

(+), add, (-), sub, (*), mul, (/), div, div', mod, mod'
  :: (Expr a, Expr b) => a -> b -> Term
(+) a b = op ADD (a, b) ()
(-) a b = op SUB (a, b) ()
(*) a b = op MUL (a, b) ()
(/) a b = op DIV (a, b) ()
add = (+)
sub = (-)
mul = (*)
div = (/)
div' = (/)
mod a b = op MOD (a, b) ()
mod' = mod

or, or', and, and' :: (Expr a, Expr b) => a -> b -> Term
or a b = op ANY (a, b) ()
and a b = op ALL (a, b) ()
or' = or
and' = and

(==), (!=), eq, ne :: (Expr a, Expr b) => a -> b -> Term
eq a b = op EQ (a, b) ()
ne a b = op NE (a, b) ()
(==) = eq
(!=) = ne

(>), (>=), (<), (<=), gt, lt, ge, le
  :: (Expr a, Expr b) => a -> b -> Term
gt a b = op GT (a, b) ()
lt a b = op LT (a, b) ()
ge a b = op GE (a, b) ()
le a b = op LE (a, b) ()
(>) = gt
(>=) = ge
(<) = lt
(<=) = le

not, not' :: (Expr a) => a -> Term
not a = op NOT [a] ()
not' = not

-- * Lists and Streams

count :: (Expr a) => a -> Term
count e = op COUNT [e] ()

(++), concat :: (Expr a, Expr b) => a -> b -> Term
(++) a b = op UNION (a, b) ()
concat = (++)

map, map' :: (Expr a) => (Term -> Term) -> a -> Term
map f a = op MAP (a, f) ()
map' = map

filter', filter :: (Expr a) => (Term -> Term) -> a -> Term
filter f a = op FILTER (a, f) ()
filter' = filter

between :: (Expr a, Expr b, Expr s) => a -> b -> s -> Term
between a b e = op BETWEEN [e] ["left_bound" := a, "right_bound" := b]

append :: (Expr a, Expr b) => a -> b -> Term
append a b = op APPEND (b, a) ()

concatMap, concatMap' :: (Expr a)
  => (Term -> Term) -> a -> Term
concatMap f e = op CONCATMAP (e, f) ()
concatMap' = concatMap

innerJoin, outerJoin :: (Expr a, Expr b)
          => (Term -> Term) -> a -> b -> Term
innerJoin f a b = op INNER_JOIN (a, b, f) ()
outerJoin f a b = op OUTER_JOIN (a, b, f) ()

eqJoin :: (Expr a, Expr b) => a -> Key -> b -> Term
eqJoin a k b = op EQ_JOIN (a, expr k, b) ()

drop, drop' :: (Expr a, Expr b) => a -> b -> Term
drop a b = op SKIP (b, a) ()
drop' = drop

take, take' :: (Expr a, Expr b) => a -> b -> Term
take a b = op LIMIT (a, b) ()
take' = take

slice :: (Expr a, Expr b, Expr c) => a -> b -> c -> Term
slice n m s = op SLICE (s, n, m) ()

(!!), nth :: (Expr a, Expr b) => a -> b -> Term
nth n s = op NTH (s, n) ()
s !! n = op NTH (s, n) ()

fold :: (Expr b, Expr s) => (Term -> Term -> Term) -> b -> s -> Term
fold f b s = op REDUCE (f, s) ["base" := b]

fold1 :: (Expr s) => (Term -> Term -> Term) -> s -> Term
fold1 f s = op REDUCE (f, s) ()

distinct :: (Expr s) => s -> Term
distinct s = op DISTINCT [s] ()

forEach :: (Expr s) => s -> (Term -> Term) -> Term
forEach s f = op FOREACH (s, f) ()

mergeRightLeft :: (Expr a) => a -> Term
mergeRightLeft a = op ZIP [a] ()

data Order = Asc  { orderAttr :: Key }
           | Desc { orderAttr :: Key }

orderBy :: (Expr s) => [Order] -> s -> Term
orderBy o s = Term $ do
  s' <- baseTerm (expr s)
  o' <- baseArray $ arr $ P.map buildOrder o
  return $ BaseTerm ORDERBY P.Nothing (s' : o') []
  where
    buildOrder (Asc k) = op ASC [k] ()
    buildOrder (Desc k) = op DESC [k] ()

groupBy, groupBy' :: (Term -> Term) -> (Term -> Term) -> Term
groupBy g mr = Term $ do
  (m, r, f) <- termToMapReduce mr
  baseTerm $ op MAP (op GROUPED_MAP_REDUCE (g, m, r) (), f) ()
groupBy' = groupBy

sum, sum' :: (Expr s) => s -> Term
sum = fold ((+) :: Term -> Term -> Term) (0 :: Term)
sum' = sum

avg :: (Expr s) => s -> Term
avg s = sum s / count s

-- * Accessors

(!) :: (Expr s) => s -> Key -> Term
(!) s k = op GETATTR (s, k) ()

pluck :: (Expr o) => [Key] -> o -> Term
pluck ks e = op PLUCK (cons e $ arr (P.map expr ks)) ()

without :: (Expr o) => [Key] -> o -> Term
without ks e = op WITHOUT (cons e $ arr (P.map expr ks)) ()

member :: (Expr o) => [Key] -> o -> Term
member ks o = op CONTAINS (cons o $ arr (P.map expr ks)) ()

merge :: (Expr a, Expr b) => a -> b -> Term
merge a b = op MERGE (a, b) ()

class Javascript r where
  js :: P.String -> r

instance Javascript Term where
  js s = op JAVASCRIPT [str s] ()

instance Javascript (Term -> Term) where
  js s x = op FUNCALL (op JAVASCRIPT [str s] (), x) ()

instance Javascript (Term -> Term -> Term) where
  js s x y = op FUNCALL (op JAVASCRIPT [str s] (), x, y) ()

if' :: (Expr a, Expr b, Expr c) => a -> b -> c -> Term
if' a b c = op BRANCH (a, b, c) ()

error, error' :: (Expr s) => s -> Term
error m = op ERROR [m] ()
error' = error

-- | Create a Database reference
db :: Text -> O.Database
db s = O.Database s

-- | Create a database on the server
dbCreate :: P.String -> Term
dbCreate db_name = op DB_CREATE [str db_name] ()

-- | Drop a database
dbDrop :: Database -> Term
dbDrop (O.Database name) = op DB_DROP [name] ()

-- | List the databases on the server
--
dbList :: Term
dbList = op DB_LIST () ()

-- | Create a simple table refence with no associated database
table :: Text -> Table
table n = O.Table Nothing n Nothing

-- | Create a table on the server
tableCreate :: Table -> TableCreateOptions -> Term
tableCreate (O.Table mdb table_name pkey) opts =
  op TABLE_CREATE (MaybeDatabase mdb, table_name) $ catMaybes [
    ("datacenter" :=) <$> tableDataCenter opts,
    ("cache_size" :=) <$> tableCacheSize opts,
    ("primary_key" :=) <$> pkey ]

-- | Drop a table
tableDrop :: Table -> Term
tableDrop (O.Table mdb table_name _) =
  op TABLE_DROP (MaybeDatabase mdb, table_name) ()

-- | List the tables in a database
tableList :: Database -> Term
tableList (O.Database name) = op DB_LIST [name] ()

get :: (Expr s, Expr k) => k -> s -> Term
get k e = op FUNCALL (\(x :: Term) ->
                       if' (x == ())
                           (error $ str "The document does not exist")
                            x,
                      op GET (e, k) ()) ()

insert :: (Expr table, Expr object) => object -> table -> Term
insert a tb = op INSERT (tb, a) ()

upsert :: (Expr table, Expr object) => object -> table -> Term
upsert a tb = op INSERT (tb, a) ["upsert" := P.True]

update :: (Expr selection) => (Term -> Term) -> selection -> Term
update f s = op UPDATE (s, f) ()

replace :: (Expr selection) => (Term -> Term) -> selection -> Term
replace f s = op REPLACE (s, f) ()

delete :: (Expr selection) => selection -> Term
delete s = op DELETE [s] ()
