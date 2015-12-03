{-# LANGUAGE FlexibleInstances, OverloadedStrings, GADTs #-}

-- | ReQL Functions
--
-- ReQL was designed for dynamic languages. Many operations take
-- optional positional and named arguments.
--
-- Optional named arguments can be added using `ex`, for example
-- `upsert = ex insert ["conflict" := "update"]`
--
-- For optional positional arguments this module defines an extra
-- function if the functionality is not available otherwise. For
-- example `argmax` for `max` and `splitOn` for `split` but `skip`
-- instead of `sliceFrom` and `avg . (!k)` instead of `avgOf k`.

module Database.RethinkDB.Functions where

import Data.Text (Text)
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import Data.Default
import Data.Monoid

import Database.RethinkDB.Wire.Term as Term
import Database.RethinkDB.ReQL
import {-# SOURCE #-} Database.RethinkDB.MapReduce
import Database.RethinkDB.Types
import Database.RethinkDB.Datum hiding (Error)

import Prelude (($), (.))
import qualified Prelude as P

-- $setup
--
-- Get the doctests ready
--
-- >>> :load Database.RethinkDB.Doctest
-- >>> import qualified Database.RethinkDB as R
-- >>> :set -XOverloadedStrings
-- >>> default (Datum, ReQL, String, Int, Double)
-- >>> h <- doctestConnect

-- $init_doctests
-- >>> try' $ run' h $ dbCreate "doctests"
-- >>> try' $ run' h $ tableCreate "foo"
-- >>> try' $ run' h $ delete $ table "foo"
-- >>> try' $ run' h $ tableCreate "bar"
-- >>> try' $ run' h $ delete $ table "bar"
-- >>> try' $ run' h $ tableDrop "bar"
-- >>> try' $ run' h $ tableCreate (table "posts")
-- >>> try' $ run' h $ delete $ table "posts"
-- >>> try' $ run' h $ tableCreate (table "places")
-- >>> try' $ run' h $ delete $ table "places"
-- >>> try' $ run' h $ tableCreate (table "users"){ tablePrimaryKey = Just "name" }
-- >>> try' $ run' h $ delete $ table "users"
-- >>> try' $ run' h $ table "users" # indexDrop "occupation"
-- >>> try' $ run' h $ table "users" # indexDrop "location"
-- >>> try' $ run' h $ table "users" # indexDrop "friends"

-- | Create a table on the server
--
-- > >>> run' h $ tableCreate (table "posts") def
-- > [{"created":1}]
-- > >>> run' h $ tableCreate (table "users"){ tablePrimaryKey = Just "name" } def
-- > [{"created":1}]
-- > >>> run' h $ tableCreate (Table (Just "doctests") "bar" (Just "name")) def
-- > [{"created":1}]
-- > >>> run' h $ ex tableCreate ["datacenter":="orion"] (Table (Just "doctests") "bar" (Just "name")) def
-- > [{"created":1}]
tableCreate :: Table -> ReQL
tableCreate (Table mdb table_name pkey) =
  withQuerySettings $ \QuerySettings{ queryDefaultDatabase = ddb } ->
    op' TABLE_CREATE (fromMaybe ddb mdb, table_name) $ catMaybes [
      ("primary_key" :=) <$> pkey ]

-- | Insert a document or a list of documents into a table
--
-- >>> run h $ table "users" # insert (map (\x -> ["name":=x]) ["bill", "bob", "nancy" :: Text]) :: IO WriteResponse
-- {inserted:3}
-- >>> run h $ table "posts" # insert ["author" := str "bill", "message" := str "hi", "id" := 1] :: IO WriteResponse
-- {inserted:1}
-- >>> run h $ table "posts" # insert ["author" := str "bill", "message" := str "hello", "id" := 2, "flag" := str "deleted"] :: IO WriteResponse
-- {inserted:1}
-- >>> run h $ table "posts" # insert ["author" := str "bob", "message" := str "lorem ipsum", "id" := 3, "flag" := str "pinned"] :: IO WriteResponse
-- {inserted:1}
insert :: (Expr object) => object -> Table -> ReQL
insert a tb = op INSERT (tb, a)

-- | Add to or modify the contents of a document
--
-- >>> run h $ table "users" # getAll "name" [str "bob"] # update (const ["occupation" := str "tailor"]) :: IO WriteResponse
-- {replaced:1}
update :: (Expr selection, Expr a) => (ReQL -> a) -> selection -> ReQL
update f s = op UPDATE (s, expr . f)

-- | Replace a document with another
--
-- >>> run h $ replace (\user -> ["name" := user!"name", "occupation" := str "clothier"]) . R.filter ((R.== str "tailor") . (!?"occupation")) $ table "users" :: IO WriteResponse
-- {replaced:1}
replace :: (Expr selection, Expr a) => (ReQL -> a) -> selection -> ReQL
replace f s = op REPLACE (s, expr . f)

-- | Delete the documents
--
-- >>> run h $ delete . getAll "name" [str "bob"] $ table "users" :: IO WriteResponse
-- {deleted:1}
delete :: (Expr selection) => selection -> ReQL
delete s = op Term.DELETE [s]

-- | Like map but for write queries
--
-- >>> _ <- run' h $ table "users" # replace (without ["post_count"])
-- >>> run h $ forEach (\user -> table "users" # get (user!"name") # ex update [nonAtomic] (const ["post_count" := R.count (table "posts" # R.filter (\post -> post!"author" R.== user!"name"))])) (table "users") :: IO WriteResponse
-- {replaced:2}
forEach :: (Expr a, Expr s) => (ReQL -> a) -> s -> ReQL
forEach f s = op FOR_EACH (s, expr P.. f)

-- | A table
--
-- >>> fmap sort $ run h $ table "users" :: IO [Datum]
-- [{"post_count":2,"name":"bill"},{"post_count":0,"name":"nancy"}]
table :: Text -> Table
table n = Table Nothing n Nothing

-- | Drop a table
--
-- >>> run' h $ tableDrop (table "foo")
-- {"config_changes":[{"new_val":null,"old_val":{"primary_key":"id","write_acks":"majority","durability":"hard","name":"foo","shards":...,"id":...,"db":"doctests"}}],"tables_dropped":1}
tableDrop :: Table -> ReQL
tableDrop (Table mdb table_name _) =
  withQuerySettings $ \QuerySettings{ queryDefaultDatabase = ddb } ->
    op TABLE_DROP (fromMaybe ddb mdb, table_name)

-- | List the tables in a database
--
-- >>> fmap sort $ run h $ tableList (db "doctests") :: IO [String]
-- ["places","posts","users"]
tableList :: Database -> ReQL
tableList name = op TABLE_LIST [name]

infixl 6 +, -
infixl 7 *, /

-- | Addition or concatenation
--
-- Use the Num instance, or a qualified operator.
--
-- >>> run h $ 2 + 5
-- 7
-- >>> run h $ str "foo" R.+ str "bar"
-- "foobar"
(+) :: (Expr a, Expr b) => a -> b -> ReQL
(+) a b = op ADD (a, b)

-- | Subtraction
--
-- >>> run h $ 2 - 5
-- -3
(-) :: (Expr a, Expr b) => a -> b -> ReQL
(-) a b = op SUB (a, b)

-- | Multiplication
--
-- >>> run h $ 2 * 5
-- 10
(*) :: (Expr a, Expr b) => a -> b -> ReQL
(*) a b = op MUL (a, b)

-- | Division
--
-- >>> run h $ 2 R./ 5
-- 0.4
(/) :: (Expr a, Expr b) => a -> b -> ReQL
(/) a b = op DIV (a, b)

-- | Mod
--
-- >>> run h $ 5 `mod` 2
-- 1
mod :: (Expr a, Expr b) => a -> b -> ReQL
mod a b = op MOD (a, b)

infixr 2 ||
infixr 3 &&

-- | Boolean or
--
-- >>> run h $ True R.|| False
-- true
(||) :: (Expr a, Expr b) => a -> b -> ReQL
a || b = op OR (a, b)

-- | Boolean and
--
-- >>> run h $ True R.&& False
-- false
(&&) :: (Expr a, Expr b) => a -> b -> ReQL
a && b = op AND (a, b)

infix 4 ==, /=

-- | Test for equality
--
-- >>> run h $ ["a" := 1] R.== ["a" := 1]
-- true
(==) :: (Expr a, Expr b) => a -> b -> ReQL
a == b = op EQ (a, b)

-- | Test for inequality
--
-- >>> run h $ 1 R./= False
-- true
(/=) :: (Expr a, Expr b) => a -> b -> ReQL
a /= b = op NE (a, b)

infix 4 >, <, <=, >=

-- | Greater than
--
-- >>> run h $ 3 R.> 2
-- true
(>) :: (Expr a, Expr b) => a -> b -> ReQL
a > b = op GT (a, b)

-- | Lesser than
--
-- >>> run h $ (str "a") R.< (str "b")
-- true
(<) :: (Expr a, Expr b) => a -> b -> ReQL
a < b = op LT (a, b)

-- | Greater than or equal to
--
-- >>> run h $ [1] R.>= Null
-- false
(>=) :: (Expr a, Expr b) => a -> b -> ReQL
a >= b = op GE (a, b)

-- | Lesser than or equal to
--
-- >>> run h $ 2 R.<= 2
-- true
(<=) :: (Expr a, Expr b) => a -> b -> ReQL
a <= b = op LE (a, b)

-- | Negation
--
-- >>> run h $ R.not False
-- true
-- >>> run h $ R.not Null
-- true
not :: (Expr a) => a -> ReQL
not a = op NOT [a]

-- * Lists and Streams

-- | The size of a sequence or an array.
--
-- >>> run h $ count (table "users")
-- 2
count :: (Expr a) => a -> ReQL
count e = op COUNT [e]

-- | Join two sequences.
--
-- >>> run h $ [1,2,3] `union` ["a", "b", "c" :: Text]
-- [1,2,3,"a","b","c"]
union :: (Expr a, Expr b) => a -> b -> ReQL
union a b = op UNION (a, b)

-- | Map a function over a sequence
--
-- >>> run h $ R.map (!"a") [["a" := 1], ["a" := 2]]
-- [1,2]
map :: (Expr a, Expr b) => (ReQL -> b) -> a -> ReQL
map f a = op MAP (a, expr P.. f)

-- | Filter a sequence given a predicate
--
-- >>> run h $ R.filter (R.< 4) [3, 1, 4, 1, 5, 9, 2, 6]
-- [3,1,1,2]
filter :: (Expr predicate, Expr seq) => predicate -> seq -> ReQL
filter f a = op' FILTER (a, f) ["default" := op ERROR ()]

-- | Query all the documents whose value for the given index is in a given range
--
-- >>> run h $ table "users" # between "name" (Closed $ str "a") (Open $ str "c")
-- [{"post_count":2,"name":"bill"}]
between :: (Expr left, Expr right, Expr seq) => Index -> Bound left -> Bound right -> seq -> ReQL
between i a b e =
  op' BETWEEN [expr e, expr $ getBound a, expr $ getBound b] $
  idx P.++ ["left_bound" ?:= closedOrOpen a, "right_bound" ?:= closedOrOpen b]
  where idx = case i of PrimaryKey -> []; Index name -> ["index" := name]

-- | Append a datum to a sequence
--
-- >>> run h $ append 3 [1, 2]
-- [1,2,3]
append :: (Expr a, Expr b) => a -> b -> ReQL
append a b = op APPEND (b, a)

-- | Map a function of a sequence and concat the results
--
-- >>> run h $ concatMap id [[1, 2], [3], [4, 5]]
-- [1,2,3,4,5]
concatMap :: (Expr a, Expr b) => (ReQL -> b) -> a -> ReQL
concatMap f e = op CONCAT_MAP (e, expr P.. f)

-- | SQL-like inner join of two sequences
--
-- >>> sorted $ run' h $ innerJoin (\user post -> user!"name" R.== post!"author") (table "users") (table "posts") # R.zip # orderBy [asc "id"] # pluck ["name", "message"]
-- [{"name":"bill","message":"hello"},{"name":"bill","message":"hi"}]
innerJoin :: (Expr a, Expr b, Expr c) => (ReQL -> ReQL -> c) -> a -> b -> ReQL
innerJoin f a b = op INNER_JOIN (a, b, fmap expr P.. f)

-- | SQL-like outer join of two sequences
--
-- >>> sorted $ run' h $ outerJoin (\user post -> user!"name" R.== post!"author") (table "users") (table "posts") # R.zip # orderBy [asc "id", asc "name"] # pluck ["name", "message"]
-- [{"name":"bill","message":"hello"},{"name":"bill","message":"hi"},{"name":"nancy"}]
outerJoin :: (Expr a, Expr b, Expr c) => (ReQL -> ReQL -> c) -> a -> b -> ReQL
outerJoin f a b = op OUTER_JOIN (a, b, fmap expr P.. f)

-- | An efficient inner_join that uses a key for the left table and an index for the right table.
--
-- >>> sorted $ run' h $ table "posts" # eqJoin "author" (table "users") "name" # R.zip # orderBy [asc "id"] # pluck ["name", "message"]
-- [{"name":"bill","message":"hello"},{"name":"bill","message":"hi"}]
eqJoin :: (Expr fun, Expr right, Expr left) => fun -> right -> Index -> left -> ReQL
eqJoin key right (Index idx) left = op' EQ_JOIN (left, key, right) ["index" := idx]
eqJoin key right PrimaryKey left = op EQ_JOIN (left, key, right)

-- | Drop elements from the head of a sequence.
--
-- >>> run h $ skip 2 [1, 2, 3, 4]
-- [3,4]
skip :: (Expr n, Expr seq) => n -> seq -> ReQL
skip a b = op SKIP (b, a)

-- | Limit the size of a sequence.
--
-- >>> run h $ limit 2 [1, 2, 3, 4]
-- [1,2]
limit :: (Expr n, Expr seq) => n -> seq -> ReQL
limit n s = op LIMIT (s, n)

-- | Cut out part of a sequence
--
-- >>> run h $ slice 2 4 [1, 2, 3, 4, 5]
-- [3,4]
slice :: (Expr a, Expr b, Expr c) => a -> b -> c -> ReQL
slice n m s = op SLICE (s, n, m)

-- | Reduce a sequence to a single value
--
-- >>> run h $ reduce0 (+) 0 [1, 2, 3]
-- 6
reduce0 :: (Expr base, Expr seq, Expr a) => (ReQL -> ReQL -> a) -> base -> seq -> ReQL
reduce0 f b s = op REDUCE (s `union` [b], fmap expr P.. f)

-- | Reduce a non-empty sequence to a single value
--
-- >>> run h $ reduce (+) [1, 2, 3]
-- 6
reduce :: (Expr a, Expr s) => (ReQL -> ReQL -> a) -> s -> ReQL
reduce f s = op REDUCE (s, fmap expr P.. f)

-- | Filter out identical elements of the sequence
--
-- >>> fmap sort $ run h $ distinct (table "posts" ! "flag") :: IO [String]
-- ["deleted","pinned"]
distinct :: (Expr s) => s -> ReQL
distinct s = op DISTINCT [s]

-- | Merge the "left" and "right" attributes of the objects in a sequence.
--
-- >>> fmap sort $ run h $ table "posts" # eqJoin "author" (table "users") "name" # R.zip :: IO [Datum]
-- [{"post_count":2,"flag":"deleted","name":"bill","author":"bill","id":2,"message":"hello"},{"post_count":2,"name":"bill","author":"bill","id":1,"message":"hi"}]
zip :: (Expr a) => a -> ReQL
zip a = op ZIP [a]

-- | Order a sequence by the given keys
--
-- >>> run' h $ table "users" # orderBy [desc "post_count", asc "name"] # pluck ["name", "post_count"]
-- [{"post_count":2,"name":"bill"},{"post_count":0,"name":"nancy"}]
--
-- >>> run' h $ table "users" # ex orderBy ["index":="name"] [] # pluck ["name"]
-- [{"name":"bill"},{"name":"nancy"}]
orderBy :: (Expr s) => [ReQL] -> s -> ReQL
orderBy o s = op ORDER_BY (expr s : P.map expr o)

-- | Ascending order
asc :: ReQL -> ReQL
asc f = op ASC [f]

-- | Descending order
desc :: ReQL -> ReQL
desc f = op DESC [f]

-- | Turn a grouping function and a reduction function into a grouped map reduce operation
--
-- >>> run' h $ table "posts" # orderBy [asc "id"] # group (!"author") (reduce (\a b -> a + "\n" + b) . R.map (!"message"))
-- [{"group":"bill","reduction":"hi\nhello"},{"group":"bob","reduction":"lorem ipsum"}]
-- >>> run' h $ table "users" # group ((!0) . splitOn "" . (!"name")) (\users -> let pc = users!"post_count" in [avg pc, R.sum pc])
-- [{"group":"b","reduction":[2,2]},{"group":"n","reduction":[0,0]}]
group ::
  (Expr group, Expr reduction, Expr seq)
  => (ReQL -> group) -> (ReQL -> reduction) -> seq -> ReQL
group g f s = ReQL $ do
  mr <- termToMapReduce (expr . f)
  runReQL $ op UNGROUP [mr $ op GROUP (expr s, expr . g)]

-- | Rewrite multiple reductions into a single map/reduce operation
mapReduce :: (Expr reduction, Expr seq) => (ReQL -> reduction) -> seq -> ReQL
mapReduce f s = ReQL $ do
  mr <- termToMapReduce (expr . f)
  runReQL $ mr (expr s)

-- | The sum of a sequence
--
-- >>> run h $ sum [1, 2, 3]
-- 6
sum :: (Expr s) => s -> ReQL
sum s = op SUM [s]

-- | The average of a sequence
--
-- >>> run h $ avg [1, 2, 3, 4]
-- 2.5
avg :: (Expr s) => s -> ReQL
avg s = op AVG [s]

-- | Minimum value
min :: Expr s => s -> ReQL
min s = op MIN [s]

-- | Value that minimizes the function
argmin :: (Expr s, Expr a) => (ReQL -> a) -> s -> ReQL
argmin f s = op MIN (s, expr . f)

-- | Minimum value
max :: Expr s => s -> ReQL
max s = op MAX [s]

-- | Floor rounds number to interger below
--
-- >>> run h $ R.floor 2.9
-- 2
floor :: Expr s => s -> ReQL
floor s = op FLOOR [s]

-- | Ceil rounds number to integer above
--
-- >>> run h $ R.ceil 2.1
-- 3
ceil :: Expr s => s -> ReQL
ceil s = op CEIL [s]

-- | Round rounds number to nearest integer
--
-- >>> run h $ R.round 2.5
-- 3
round :: Expr s => s -> ReQL
round s = op ROUND [s]

-- | Value that maximizes the function
argmax :: (Expr s, Expr a) => (ReQL -> a) -> s -> ReQL
argmax f s = op MAX (s, expr . f)

-- * Accessors

infixl 9 !

-- | Get a single field from an object or an element of an array
--
-- >>> run h $ ["foo" := True] ! "foo"
-- true
--
-- >>> run h $ [1, 2, 3] ! 0
-- 1
--
-- Or a single field from each object in a sequence
--
-- >>> run h $ [["foo" := True], ["foo" := False]] ! "foo"
-- [true,false]
(!) :: (Expr s) => s -> ReQL -> ReQL
s ! k = op BRACKET (s, k)

-- | Get a single field, or null if not present
--
-- >>> run' h $ empty !? "foo"
-- null
(!?) :: (Expr s) => s -> ReQL -> ReQL
s !? k = P.flip apply [expr s, k] $ \s' k' -> op DEFAULT (op BRACKET (s', k'), Null)

-- | Keep only the given attributes
--
-- >>> run' h $ [["a" := 1, "b" := 2], ["a" := 2, "c" := 7], ["b" := 4]] # pluck ["a"]
-- [{"a":1},{"a":2},{}]
pluck :: (Expr o) => [ReQL] -> o -> ReQL
pluck ks e = op PLUCK (cons e $ arr (P.map expr ks))

-- | Remove the given attributes from an object
--
-- >>> run' h $ [["a" := 1, "b" := 2], ["a" := 2, "c" := 7], ["b" := 4]] # without ["a"]
-- [{"b":2},{"c":7},{"b":4}]
without :: (Expr o) => [ReQL] -> o -> ReQL
without ks e = op WITHOUT (cons e $ arr (P.map expr ks))

-- | Test if a sequence contains a given element
--
-- >>> run' h $ [1,2,3] # contains 1
-- true
contains :: (Expr x, Expr seq) => x -> seq -> ReQL
contains x s = op CONTAINS (s, x)

-- | Merge two objects together
--
-- >>> run' h $ merge ["a" := 1, "b" := 1] ["b" := 1, "c" := 2]
-- {"a":1,"b":1,"c":2}
merge :: (Expr a, Expr b) => a -> b -> ReQL
merge a b = op MERGE (b, a)

-- | Literal objects, in a merge or update, are not processed recursively.
--
-- >>> run' h $ ["a" := ["b" := 1]] # merge ["a" := literal ["c" := 2]]
-- {"a":{"c":2}}
literal :: Expr a => a -> ReQL
literal a = op LITERAL [a]

-- | Remove fields when doing a merge or update
--
-- >>> run' h $ ["a" := ["b" := 1]] # merge ["a" := remove]
-- {}
remove :: ReQL
remove = op LITERAL ()

-- | Evaluate a JavaScript expression
--
-- >>> run' h $ js "Math.PI"
-- 3.141592653589793
-- >>> let r_sin x = js "Math.sin" `apply` [x]
-- >>> run h $ R.map r_sin [pi, pi/2]
-- [1.2246...,1]
js :: ReQL -> ReQL
js s = op JAVASCRIPT [s]

-- | Server-side if
--
-- >>> run h $ branch (1 R.< 2) 3 4
-- 3
branch :: (Expr a, Expr b, Expr c) => a -> b -> c -> ReQL
branch a b c = op BRANCH (a, b, c)

-- | Abort the query with an error
--
-- >>> run' h $ R.error (str "haha") R./ 2 + 1
-- *** Exception: RethinkDB: Runtime error: "haha"
--   in add(div({- HERE -} error("haha"), 2), 1)
error :: (Expr s) => s -> ReQL
error m = op ERROR [m]

-- | Create a Database reference
--
-- >>> run' h $ db "test" # info
-- {"name":"test","id":...,"type":"DB"}
db :: Text -> Database
db = Database

-- | Create a database on the server
--
-- >>> run' h $ dbCreate "dev"
-- {"config_changes":[{"new_val":{"name":"dev","id":...},"old_val":null}],"dbs_created":1}
dbCreate :: Text -> ReQL
dbCreate db_name = op DB_CREATE [expr db_name]

-- | Drop a database
--
-- >>> run' h $ dbDrop (db "dev")
-- {"config_changes":[{"new_val":null,"old_val":{"name":"dev","id":...}}],"tables_dropped":0,"dbs_dropped":1}
dbDrop :: Database -> ReQL
dbDrop (Database name) = op DB_DROP [name]

-- | List the databases on the server
--
-- >>> _ <- run' h $ dbList
dbList :: ReQL
dbList = op DB_LIST ()

-- | Create an index on the table from the given function
--
-- >>> run' h $ table "users" # indexCreate "occupation" (!"occupation")
-- {"created":1}
-- >>> run' h $ table "users" # ex indexCreate ["multi":=True] "friends" (!"friends")
-- {"created":1}
-- >>> run' h $ table "users" # ex indexCreate ["geo":=True] "location" (!"location")
-- {"created":1}
indexCreate :: (Expr fun) => Text -> fun -> Table -> ReQL
indexCreate name f tbl = op INDEX_CREATE (tbl, expr name, f)

-- | Get the status of the given indexes
--
-- > run' h $ table "users" # indexStatus []
indexStatus :: Expr table => [ReQL] -> table -> ReQL
indexStatus ixes tbl = op INDEX_STATUS (tbl, op ARGS [ixes])

-- | Wait for an index to be built
--
-- > run' h $ table "users" # indexWait []
indexWait :: Expr table => [ReQL] -> table -> ReQL
indexWait ixes tbl = op INDEX_STATUS (tbl, op ARGS [ixes])

indexRename :: Expr table => ReQL -> ReQL -> table -> ReQL
indexRename from to tbl = op INDEX_RENAME (tbl, from, to)

-- | Ensures that writes on a given table are written to permanent storage
--
-- >>> run' h $ sync (table "users")
-- {"synced":1}
sync :: Expr table => table -> ReQL
sync tbl = op SYNC [tbl]

-- | List the indexes on the table
--
-- >>> run' h $ indexList (table "users")
-- ["friends","location","occupation"]
indexList :: Table -> ReQL
indexList tbl = op INDEX_LIST [tbl]

-- | Drop an index
--
-- >>> run' h $ table "users" # indexDrop "occupation"
-- {"dropped":1}
indexDrop :: Key -> Table -> ReQL
indexDrop name tbl = op INDEX_DROP (tbl, name)

-- | Retreive documents by their indexed value
--
-- >>> run' h $ table "users" # getAll PrimaryKey [str "bill"]
-- [{"post_count":2,"name":"bill"}]
getAll :: (Expr values) => Index -> values -> Table -> ReQL
getAll idx xs tbl =
  op' GET_ALL (tbl, op ARGS [xs]) $
  case idx of
    Index i -> ["index" := i]
    PrimaryKey -> []

-- | Get a document by primary key
--
-- >>> run' h $ table "users" # get "nancy"
-- {"post_count":0,"name":"nancy"}
get :: Expr s => ReQL -> s -> ReQL
get k e = op Term.GET (e, k)

-- | Convert a value to a different type
--
-- >>> run h $ coerceTo "STRING" 1
-- "1"
coerceTo :: (Expr x) => ReQL -> x -> ReQL
coerceTo t a = op COERCE_TO (a, t)

-- | Convert a value to an array
--
-- >>> run h $ asArray $ ["a" := 1, "b" := 2] :: IO [(String, Int)]
-- [("a",1),("b",2)]
asArray :: Expr x => x -> ReQL
asArray = coerceTo "ARRAY"

-- | Convert a value to a string
--
-- >>> run h $ asString $ ["a" := 1, "b" := 2]
-- "{\"a\":1,\"b\":2}"
asString :: Expr x => x -> ReQL
asString = coerceTo "STRING"

-- | Convert a value to a number
--
-- >>> run h $ asNumber (str "34")
-- 34
asNumber :: Expr x => x -> ReQL
asNumber = coerceTo "NUMBER"

-- | Convert a value to an object
--
-- >>> run' h $ asObject $ [(str "a",1),("b",2)]
-- {"a":1,"b":2}
asObject :: Expr x => x -> ReQL
asObject = coerceTo "OBJECT"

-- | Convert a value to a boolean
asBool :: Expr x => x -> ReQL
asBool = coerceTo "BOOL"

-- | Like hasFields followed by pluck
--
-- >>> run' h $ [["a" := 1, "b" := 2], ["a" := 2, "c" := 7], ["b" := 4]] # withFields ["a"]
-- [{"a":1},{"a":2}]
withFields :: Expr seq => [ReQL] -> seq -> ReQL
withFields p s = op WITH_FIELDS (s, p)

-- | The position in the sequence of the elements that match the predicate
--
-- >>> run h $ indexesOf (match "ba.") [str "foo", "bar", "baz"]
-- [1,2]
indexesOf :: (Expr fun, Expr seq) => fun -> seq -> ReQL
indexesOf f s = op OFFSETS_OF (s, f)

-- | Test if a sequence is empty
--
-- >>> run h $ isEmpty [1]
-- false
isEmpty :: Expr seq => seq -> ReQL
isEmpty s = op IS_EMPTY [s]

-- | Select a given number of elements from a sequence with uniform random distribution
--
-- >>> _ <- run' h $ sample 3 [0,1,2,3,4,5,6,7,8,9]
sample :: (Expr n, Expr seq) => n -> seq -> ReQL
sample n s = op SAMPLE (s, n)

-- | Prepend an element to an array
--
-- >>> run h $ prepend 1 [2,3]
-- [1,2,3]
prepend :: (Expr datum, Expr array) => datum -> array -> ReQL
prepend d a = op PREPEND (a, d)

-- | The different of two lists
--
-- >>> run h $ [1,2,3,4,5] # difference [2,5]
-- [1,3,4]
difference :: (Expr a, Expr b) => a -> b -> ReQL
difference a b = op DIFFERENCE (b, a)

-- | Insert a datum into an array if it is not yet present
--
-- >>> run h $ setInsert 3 [1,2,4,4,5]
-- [1,2,4,5,3]
setInsert :: (Expr datum, Expr array) => datum -> array -> ReQL
setInsert d a = op SET_INSERT (a, d)

-- | The union of two sets
--
-- >>> run h $ [1,2] `setUnion` [2,3]
-- [2,3,1]
setUnion :: (Expr a, Expr b) => a -> b -> ReQL
setUnion a b = op SET_UNION (b, a)

-- | The intersection of two sets
--
-- >>> run h $ [1,2] `setIntersection` [2,3]
-- [2]
setIntersection :: (Expr a, Expr b) => a -> b -> ReQL
setIntersection a b = op SET_INTERSECTION (b, a)

-- | The difference of two sets
--
-- >>> run h $ [2,3] # setDifference [1,2]
-- [3]
setDifference :: (Expr set, Expr remove) => remove -> set -> ReQL
setDifference r s = op SET_DIFFERENCE (s, r)

-- | Test if an object has the given fields
--
-- >>> run h $ hasFields "a" $ ["a" := 1]
-- true
hasFields :: (Expr obj) => ReQL -> obj -> ReQL
hasFields p o = op HAS_FIELDS (o, expr p)

-- | Insert a datum at the given position in an array
--
-- >>> run h $ insertAt 1 4 [1,2,3]
-- [1,4,2,3]
insertAt :: (Expr n, Expr datum, Expr array) => n -> datum -> array -> ReQL
insertAt n d a = op INSERT_AT (a, n, d)

-- | Splice an array at a given position inside another array
--
-- >>> run h $ spliceAt 2 [4,5] [1,2,3]
-- [1,2,4,5,3]
spliceAt :: (Expr n, Expr replace, Expr array) => n -> replace -> array -> ReQL
spliceAt n s a = op SPLICE_AT (a, n, s)

-- | Delete an element from an array
--
-- >>> run h $ deleteAt 1 [1,2,3]
-- [1,3]
deleteAt :: (Expr n, Expr array) => n -> array -> ReQL
deleteAt n a = op DELETE_AT (a, n)

-- | Change an element in an array
--
-- >>> run h $ changeAt 1 4 [1,2,3]
-- [1,4,3]
changeAt :: (Expr n, Expr datum, Expr array) => n -> datum -> array -> ReQL
changeAt n d a = op CHANGE_AT (a, n, d)

-- | The list of keys of the given object
--
-- >>> run h $ keys ["a" := 1, "b" := 2]
-- ["a","b"]
keys :: Expr object => object -> ReQL
keys o = op KEYS [o]

-- | The list of values of the given object
--
-- >>> run h $ values ["a" := 1, "b" := 2]
-- [1,2]
values :: Expr object => object -> ReQL
values o = op VALUES [o]

-- | Match a string to a regular expression.
--
-- >>> run' h $ str "foobar" # match "f(.)+[bc](.+)"
-- {"groups":[{"start":2,"end":3,"str":"o"},{"start":4,"end":6,"str":"ar"}],"start":0,"end":6,"str":"foobar"}
match :: (Expr string) => ReQL -> string -> ReQL
match r s = op MATCH (s, r)

-- | Apply a function to a list of arguments.
--
-- Called /do/ in the official drivers
--
-- >>> run h $ (\x -> x R.* 2) `apply` [4]
-- 8
apply :: (Expr fun, Expr arg) => fun -> [arg] -> ReQL
f `apply` as = op FUNCALL (expr f : P.map expr as)

-- | Catch some expections inside the query.
--
-- Called /default/ in the official drivers
--
-- >>> run h $ R.handle (const 0) $ ["a" := 1] ! "b"
-- 0
-- >>> run h $ R.handle (expr . id) $ ["a" := 1] ! "b"
-- "No attribute `b` in object:\n{\n\t\"a\":\t1\n}"
handle :: (Expr instead, Expr reql) => (ReQL -> instead) -> reql -> ReQL
handle h r = op DEFAULT (r, expr . h)

-- | A string representing the type of an expression
--
-- >>> run h $ typeOf 1
-- "NUMBER"
typeOf :: Expr a => a -> ReQL
typeOf a = op TYPE_OF [a]

-- | Get information on a given expression. Useful for tables and databases.
--
-- >>> run h $ info $ table "users"
-- {"primary_key":"name","doc_count_estimates":...,"name":"users","id":...,"indexes":["friends","location"],"type":"TABLE","db":{"name":"doctests","id":...,"type":"DB"}}
info :: Expr a => a -> ReQL
info a = op INFO [a]

-- | Parse a json string into an object
--
-- >>> run' h $ json "{\"a\":1}"
-- {"a":1}
json :: ReQL -> ReQL
json s = op Term.JSON [s]

-- | Flipped function application
infixl 8 #
(#) :: (Expr a, Expr b) =>  a -> (a -> b) -> ReQL
x # f = expr (f x)

-- | Convert to upper case
--
-- >>> run h $ upcase (str "Foo")
-- "FOO"
upcase :: Expr str => str -> ReQL
upcase s = op UPCASE [s]

-- | Convert to lower case
--
-- >>> run h $ downcase (str "Foo")
-- "foo"
downcase :: Expr str => str -> ReQL
downcase s = op DOWNCASE [s]

-- | Split a string on whitespace characters
--
-- >>> run' h $ split (str "foo bar")
-- ["foo","bar"]
split :: Expr str => str -> ReQL
split s = op SPLIT [s]

-- | Split a string on a given delimiter
--
-- >>> run' h $ str "foo, bar" # splitOn ","
-- ["foo"," bar"]
--
-- >>> run' h $ str "foo" # splitOn ""
-- ["f","o","o"]
splitOn :: Expr str => ReQL -> str -> ReQL
splitOn sep s = op SPLIT [expr s, sep]

-- | Split a string up to a given number of times
--
-- >>> run' h $ str "a:b:c:d" # splitMax ":" 2
-- ["a","b","c:d"]
splitMax :: Expr str => ReQL -> ReQL -> str -> ReQL
splitMax sep n s = op SPLIT [expr s, sep, n]

-- | A random float between 0 and 1
--
-- >>> run' h $ (\x -> x R.< 1 R.&& x R.>= 0) `apply` [random]
-- true
random :: ReQL
random = op RANDOM ()

-- | A random number between 0 and n
--
-- >>> run' h $ (\x -> x R.< 10 R.&& x R.>= 0) `apply` [randomTo 10]
-- true
randomTo :: ReQL -> ReQL
randomTo n = op RANDOM [n]

-- | A random number between 0 and n
--
-- >>> run' h $ (\x -> x R.< 10 R.&& x R.>= 5) `apply` [randomFromTo 5 10]
-- true
randomFromTo :: ReQL -> ReQL -> ReQL
randomFromTo n m = op RANDOM [n, m]

data HttpOptions = HttpOptions {
  httpTimeout :: Maybe P.Int,
  httpReattempts :: Maybe P.Int,
  httpRedirects :: Maybe P.Int,
  httpVerify :: Maybe P.Bool,
  httpResultFormat :: Maybe HttpResultFormat,
  httpMethod :: Maybe HttpMethod,
  httpAuth :: Maybe [Attribute Dynamic],
  httpParams :: Maybe [Attribute Dynamic],
  httpHeader :: Maybe [Attribute Dynamic],
  httpData :: Maybe ReQL,
  httpPage :: Maybe PaginationStrategy,
  httpPageLimit :: Maybe P.Int
  }

data HttpResultFormat =
  FormatAuto | FormatJSON | FormatJSONP | FormatBinary

instance Expr HttpResultFormat where
  expr FormatAuto = "auto"
  expr FormatJSON = "json"
  expr FormatJSONP = "jsonp"
  expr FormatBinary = "binary"

data HttpMethod = GET | POST | PUT | PATCH | DELETE | HEAD
                deriving P.Show

instance Expr HttpMethod where
  expr = str P.. P.show

data PaginationStrategy =
  LinkNext |
  PaginationFunction (ReQL -> ReQL)

instance Expr PaginationStrategy where
  expr LinkNext = "link-next"
  expr (PaginationFunction f) = expr f

instance Default HttpOptions where
  def = HttpOptions {
    httpTimeout = Nothing,
    httpReattempts  = Nothing,
    httpRedirects = Nothing,
    httpVerify = Nothing,
    httpResultFormat = Nothing,
    httpMethod = Nothing,
    httpAuth = Nothing,
    httpParams = Nothing,
    httpHeader = Nothing,
    httpData = Nothing,
    httpPage = Nothing,
    httpPageLimit = Nothing
    }

-- | Retrieve data from the specified URL over HTTP
--
-- >>> _ <- run' h $ http "http://httpbin.org/get" def{ httpParams = Just ["foo" := 1] }
-- >>> _ <- run' h $ http "http://httpbin.org/put" def{ httpMethod = Just PUT, httpData = Just $ expr ["foo" := "bar"] }
http :: Expr url => url -> HttpOptions -> ReQL
http url opts = op' HTTP [url] $ render opts
  where
    render ho =
      let
        go :: Expr x => (HttpOptions -> Maybe x) -> Text -> [Attribute Static]
        go f s = maybe [] (\x -> [s := x]) (f ho)
      in mconcat [
        go httpTimeout "timeout",
        go httpReattempts "reattempts",
        go httpRedirects "redirects",
        go httpVerify "verify",
        go httpResultFormat "result_format",
        go httpMethod "method",
        go httpAuth "auth",
        go httpParams "params",
        go httpHeader "header",
        go httpData "data",
        go httpPage "page",
        go httpPageLimit "page_limit"
        ]

-- | Splice a list of values into an argument list
args :: Expr array => array -> ReQL
args a = op ARGS [a]

-- | Return an infinite stream of objects representing changes to a table
--
-- >>> cursor <- run h $ table "posts" # changes :: IO (Cursor Datum)
-- >>> run h $ table "posts" # insert ["author" := "bill", "message" := "bye", "id" := 4] :: IO WriteResponse
-- {inserted:1}
-- >>> next cursor
-- Just {"new_val":{"author":"bill","id":4,"message":"bye"},"old_val":null}
changes :: Expr seq => seq -> ReQL
changes s = op CHANGES [s]

-- | Optional argument for returning an array of objects describing the changes made
--
-- >>> run h $ table "users" # ex insert [returnChanges] ["name" := "sabrina"] :: IO WriteResponse
-- {inserted:1,changes:[{"old_val":null,"new_val":{"name":"sabrina"}}]}
returnChanges :: Attribute a
returnChanges = "return_changes" := P.True

-- | Optional argument for changes
includeStates :: Attribute a
includeStates = "include_states" := P.True

-- | Optional argument for changes
includeInitial :: Attribute a
includeInitial = "include_initial" := P.True

data Durability = Hard | Soft

instance Expr Durability where
  expr Hard = "hard"
  expr Soft = "soft"

-- | Optional argument for soft durability writes
durability :: Durability -> Attribute a
durability d = "durability" := d

-- | Optional argument for non-atomic writes
--
-- >>> run' h $ table "users" # get "sabrina" # update (merge ["lucky_number" := random])
-- *** Exception: RethinkDB: Runtime error: "Could not prove argument deterministic.  Maybe you want to use the non_atomic flag?"
--   in
--     {- HERE -}
--     update(
--       get(table(db("doctests"), "users"), "sabrina"),
--       (\b -> merge(b, {lucky_number: random()})))
-- >>> run h $ table "users" # get "sabrina" # ex update [nonAtomic] (merge ["lucky_number" := random]) :: IO WriteResponse
-- {replaced:1}
nonAtomic :: Attribute a
nonAtomic = "non_atomic" := P.True

data ConflictResolution = Error | Replace | Update

instance Expr ConflictResolution where
  expr Error = "error"
  expr Replace = "replace"
  expr Update = "update"

conflict :: ConflictResolution -> Attribute a
conflict cr = "conflict" := cr

-- | Generate a UUID
--
-- >>> run h uuid
-- "...-...-...-..."
uuid :: ReQL
uuid = op UUID ()

-- | Generate a Version 5 UUID
--
-- >>> run h $ uuid5 "foo"
-- "aa32a020-8c2d-5ff1-823b-ad3fa5d067eb"
uuid5 :: Expr name => name -> ReQL
uuid5 name = op UUID [name]

-- | Generate numbers starting from 0
--
-- >>> run h $ range 10
-- [0,1,2,3,4,5,6,7,8,9]
range :: ReQL -> ReQL
range n = op RANGE [n]

-- | Generate numbers within a range
--
-- >>> run h $ rangeFromTo 2 4
-- [2,3]
rangeFromTo :: ReQL -> ReQL -> ReQL
rangeFromTo a b = op RANGE (a, b)

-- | Generate numbers starting from 0
--
-- >>> run' h $ rangeAll # limit 4
-- [0,1,2,3]
rangeAll :: ReQL
rangeAll = op RANGE ()

-- | Wait for tables to be ready
--
-- >>> run h $ table "users" # wait
-- {"ready":1}
wait :: Expr table => table -> ReQL
wait t = op WAIT [t]

-- | Convert an object or value to a JSON string
--
-- >>> run h $ toJSON "a"
-- "\"a\""
toJSON :: Expr a => a -> ReQL
toJSON a = op TO_JSON_STRING [a]

-- | Map over two sequences
--
-- >>> run h $ zipWith (+) [1,2] [3,4]
-- [4,6]
zipWith :: (Expr left, Expr right, Expr b)
        => (ReQL -> ReQL -> b) -> left -> right -> ReQL
zipWith f a b = op MAP (a, b, \x y -> expr (f x y))

-- | Map over multiple sequences
--
-- >>> run' h $ zipWithN (\a b c -> expr $ a + b * c) [[1,2],[3,4],[5,6]]
-- [16,26]
zipWithN :: (Arr a, Expr f)
         => f -> a -> ReQL
zipWithN f s = op MAP $ arr s <> arr [f]

-- | Change a table's configuration
--
-- >>> run h $ table "users" # reconfigure 2 1
-- {"config_changes":[{"new_val":{"primary_key":"name","write_acks":"majority","durability":"hard","name":"users","shards":...
reconfigure :: (Expr table, Expr replicas)
            => ReQL -> replicas -> table -> ReQL
reconfigure shards replicas t = op' RECONFIGURE [t] ["shards" := shards, "replicas" := replicas]

-- | Rebalance a table's shards
--
-- >>> run h $ table "users" # rebalance
-- {"rebalanced":1,"status_changes":[{"new_val":{"status":{"all_replicas_ready":...,"ready_for_outdated_reads":...
rebalance :: Expr table => table -> ReQL
rebalance t = op REBALANCE [t]

-- | Get the config for a table or database
--
-- >>> run h $ table "users" # config
-- {"primary_key":"name","write_acks":"majority","durability":"hard","name":"users","shards":...,"id":...,"db":"doctests"}
config :: Expr table => table -> ReQL
config t = op CONFIG [t]

-- | Get the status of a table
--
-- >>> run h $ table "users" # status
-- {"status":{"all_replicas_ready":true,"ready_for_outdated_reads":true,...
status :: Expr table => table -> ReQL
status t = op STATUS [t]
