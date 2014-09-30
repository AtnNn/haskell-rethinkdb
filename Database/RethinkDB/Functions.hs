{-# LANGUAGE FlexibleInstances, OverloadedStrings, GADTs #-}

-- | Functions from the ReQL (RethinkDB Query Language)

module Database.RethinkDB.Functions where

import Data.Text (Text)
import Control.Monad.State
import Control.Applicative
import Data.Maybe

import Database.RethinkDB.ReQL
import {-# SOURCE #-} Database.RethinkDB.MapReduce
import Database.RethinkDB.Objects as O

import Prelude (($), return, Double, Bool, String)
import qualified Prelude as P

-- $setup
--
-- Get the doctests ready
--
-- >>> :load Database.RethinkDB.NoClash
-- >>> import qualified Database.RethinkDB as R
-- >>> import Prelude
-- >>> import Data.Text (Text)
-- >>> import Data.Maybe
-- >>> import Control.Exception
-- >>> import Database.RethinkDB.Functions ()
-- >>> import Database.RethinkDB ()
-- >>> import Data.List (sort)
-- >>> import System.IO.Unsafe
-- >>> :set -XOverloadedStrings
-- >>> let try' x = (try x `asTypeOf` return (Left (undefined :: SomeException))) >> return ()
-- >>> h' <- unsafeInterleaveIO $ connect "localhost" 28015 def
-- >>> let h = use h' "doctests"

-- $init_doctests
-- >>> try' $ run' h' $ dbCreate "doctests"
-- >>> try' $ run' h $ tableCreate "foo" def
-- >>> try' $ run' h $ delete $ table "foo"
-- >>> try' $ run' h $ tableCreate "bar" def
-- >>> try' $ run' h $ delete $ table "bar"
-- >>> try' $ run' h $ tableDrop "bar"
-- >>> try' $ run' h $ tableCreate (table "posts") def
-- >>> try' $ run' h $ delete $ table "posts"
-- >>> try' $ run' h $ tableCreate (table "users"){ tablePrimaryKey = Just "name" } def
-- >>> try' $ run' h $ delete $ table "users"

-- | Create a table on the server
--
-- > >>> run' h $ tableCreate (table "posts") def
-- > [{"created":1}]
-- > >>> run' h $ tableCreate (table "users"){ tablePrimaryKey = Just "name" } def
-- > [{"created":1}]
-- > >>> run' h $ tableCreate (Table (Just "doctests") "bar" (Just "name")) def
-- > [{"created":1}]
tableCreate :: Table -> TableCreateOptions -> ReQL
tableCreate (O.Table mdb table_name pkey) opts =
  withQuerySettings $ \QuerySettings{ queryDefaultDatabase = ddb } ->
    op' TABLE_CREATE (fromMaybe ddb mdb, table_name) $ catMaybes [
      ("datacenter" :==) <$> tableDataCenter opts,
      ("primary_key" :==) <$> pkey ]

-- | Insert a document or a list of documents into a table
--
-- >>> run h $ table "users" # insert (map (\x -> obj ["name":=x]) ["bill", "bob", "nancy" :: Text]) :: IO (Maybe WriteResponse)
-- Just {inserted:3}
-- >>> _ <- run' h $ table "posts" # insert (obj ["author" := str "bill", "message" := str "hi", "id" := 1])
-- >>> _ <- run' h $ table "posts" # insert (obj ["author" := str "bill", "message" := str "hello", "id" := 2, "flag" := str "deleted"])
-- >>> _ <- run' h $ table "posts" # insert (obj ["author" := str "bob", "message" := str "lorem ipsum", "id" := 3, "flag" := str "pinned"])
insert :: (Expr object) => object -> Table -> ReQL
insert a tb = canReturnVals $ op INSERT (tb, a)

-- | Like insert, but replace existing documents with the same primary key
--
-- >>> run h $ table "users" # upsert (obj ["name" := str "bill", "occupation" := str "pianist"]) :: IO (Maybe WriteResponse)
-- Just {replaced:1}
upsert :: (Expr table, Expr object) => object -> table -> ReQL
upsert a tb = canReturnVals $ op' INSERT (tb, a) ["upsert" :== P.True]

-- | Add to or modify the contents of a document
--
-- >>> run h $ table "users" # getAll "name" [str "bob"] # update (const $ obj ["occupation" := str "tailor"]) :: IO (Maybe WriteResponse)
-- Just {replaced:1}
update :: (Expr selection, Expr a) => (ReQL -> a) -> selection -> ReQL
update f s = canNonAtomic $ canReturnVals $ op UPDATE (s, expr . f)

-- | Replace a document with another
--
-- >>> run h $ replace (\user -> obj ["name" := user!"name", "occupation" := str "clothier"]) . R.filter ((R.== str "tailor") . (!?"occupation")) $ table "users" :: IO (Maybe WriteResponse)
-- Just {replaced:1}
replace :: (Expr selection, Expr a) => (ReQL -> a) -> selection -> ReQL
replace f s = canNonAtomic $ canReturnVals $ op REPLACE (s, expr . f)

-- | Delete the documents
--
-- >>> run h $ delete . getAll "name" [str "bob"] $ table "users" :: IO (Maybe WriteResponse)
-- Just {deleted:1}
delete :: (Expr selection) => selection -> ReQL
delete s = canReturnVals $ op DELETE [s]

-- | Like map but for write queries
--
-- >>> _ <- run' h $ table "users" # replace (without ["post_count"])
-- >>> run h $ forEach (table "users") (\user -> table "users" # get (user!"name") # update (const $ obj ["post_count" := R.length (table "posts" # R.filter (\post -> post!"author" R.== user!"name"))])) # nonAtomic :: IO (Maybe WriteResponse)
-- Just {replaced:2}
forEach :: (Expr s, Expr a) => s -> (ReQL -> a) -> ReQL
forEach s f = op FOREACH (s, expr P.. f)

-- | A table
--
-- >>> (mapM_ print . sort =<<) $ run' h $ table "users"
-- {"post_count":0.0,"name":"nancy"}
-- {"post_count":2,"name":"bill","occupation":"pianist"}
table :: Text -> Table
table n = O.Table Nothing n Nothing

-- | Drop a table
--
-- >>> run' h $ tableDrop (table "foo")
-- [{"dropped":1}]
tableDrop :: Table -> ReQL
tableDrop (O.Table mdb table_name _) =
  withQuerySettings $ \QuerySettings{ queryDefaultDatabase = ddb } ->
    op TABLE_DROP (fromMaybe ddb mdb, table_name)

-- | List the tables in a database
--
-- >>> fmap (fmap sort) $ run h $ tableList (db "doctests") :: IO (Maybe [String])
-- Just ["posts","users"]
tableList :: Database -> ReQL
tableList name = op TABLE_LIST [name]

infixl 6 +, -
infixl 7 *, /

-- | Addition or concatenation
--
-- Use the Num instance, or a qualified operator.
--
-- >>> run h $ 2 + 5 :: IO (Maybe Int)
-- Just 7
-- >>> run h $ 2 R.+ 5 :: IO (Maybe Int)
-- Just 7
-- >>> run h $ (str "foo") + (str "bar") :: IO (Maybe String)
-- Just "foobar"
(+) :: (Expr a, Expr b) => a -> b -> ReQL
(+) a b = op ADD (a, b)

-- | Subtraction
--
-- >>> run h $ 2 - 5 :: IO (Maybe Int)
-- Just (-3)
(-) :: (Expr a, Expr b) => a -> b -> ReQL
(-) a b = op SUB (a, b)

-- | Multiplication
--
-- >>> run h $ 2 * 5 :: IO (Maybe Int)
-- Just 10
(*) :: (Expr a, Expr b) => a -> b -> ReQL
(*) a b = op MUL (a, b)

-- | Division
--
-- >>> run h $ 2 R./ 5 :: IO (Maybe Double)
-- Just 0.4
(/) :: (Expr a, Expr b) => a -> b -> ReQL
(/) a b = op DIV (a, b)

-- | Mod
--
-- >>> run h $ 5 `mod` 2 :: IO (Maybe Int)
-- Just 1
mod :: (Expr a, Expr b) => a -> b -> ReQL
mod a b = op MOD (a, b)

infixr 2 ||
infixr 3 &&

-- | Boolean or
--
-- >>> run h $ True R.|| False :: IO (Maybe Bool)
-- Just True
(||) :: (Expr a, Expr b) => a -> b -> ReQL
a || b = op ANY (a, b)

-- | Boolean and
--
-- >>> run h $ True R.&& False :: IO (Maybe Bool)
-- Just False
(&&) :: (Expr a, Expr b) => a -> b -> ReQL
a && b = op ALL (a, b)

infix 4 ==, /=

-- | Test for equality
--
-- >>> run h $ obj ["a" := 1] R.== obj ["a" := 1] :: IO (Maybe Bool)
-- Just True
(==) :: (Expr a, Expr b) => a -> b -> ReQL
a == b = op EQ (a, b)

-- | Test for inequality
--
-- >>> run h $ 1 R./= False :: IO (Maybe Bool)
-- Just True
(/=) :: (Expr a, Expr b) => a -> b -> ReQL
a /= b = op NE (a, b)

infix 4 >, <, <=, >=

-- | Greater than
--
-- >>> run h $ 3 R.> 2 :: IO (Maybe Bool)
-- Just True
(>) :: (Expr a, Expr b) => a -> b -> ReQL
a > b = op GT (a, b)

-- | Lesser than
--
-- >>> run h $ (str "a") R.< (str "b") :: IO (Maybe Bool)
-- Just True
(<) :: (Expr a, Expr b) => a -> b -> ReQL
a < b = op LT (a, b)

-- | Greater than or equal to
--
-- >>> run h $ [1] R.>= () :: IO (Maybe Bool)
-- Just False
(>=) :: (Expr a, Expr b) => a -> b -> ReQL
a >= b = op GE (a, b)

-- | Lesser than or equal to
--
-- >>> run h $ 2 R.<= 2 :: IO (Maybe Bool)
-- Just True
(<=) :: (Expr a, Expr b) => a -> b -> ReQL
a <= b = op LE (a, b)

-- | Negation
--
-- >>> run h $ R.not False :: IO (Maybe Bool)
-- Just True
-- >>> run h $ R.not () :: IO (Maybe Bool)
-- Just True
not :: (Expr a) => a -> ReQL
not a = op NOT [a]

-- * Lists and Streams

-- | The size of a sequence or an array.
--
-- Called /count/ in the official drivers
--
-- >>> run h $ R.length (table "users") :: IO (Maybe Int)
-- Just 2
length :: (Expr a) => a -> ReQL
length e = op COUNT [e]

infixr 5 ++

-- | Join two sequences.
--
-- Called /union/ in the official drivers
--
-- >>> run h $ [1,2,3] R.++ ["a", "b", "c" :: Text] :: IO (Maybe [JSON])
-- Just [1,2,3,"a","b","c"]
(++) :: (Expr a, Expr b) => a -> b -> ReQL
a ++ b = op UNION (a, b)

-- | Map a function over a sequence
--
-- >>> run h $ R.map (!"a") [obj ["a" := 1], obj ["a" := 2]] :: IO (Maybe [Int])
-- Just [1,2]
map :: (Expr a, Expr b) => (ReQL -> b) -> a -> ReQL
map f a = op MAP (a, expr P.. f)

-- | Filter a sequence given a predicate
--
-- >>> run h $ R.filter (R.< 4) [3, 1, 4, 1, 5, 9, 2, 6] :: IO (Maybe [Int])
-- Just [3,1,1,2]
filter :: (Expr predicate, Expr seq) => predicate -> seq -> ReQL
filter f a = op' FILTER (a, f) ["default" :== op ERROR ()]

-- | Query all the documents whose value for the given index is in a given range
--
-- >>> run h $ table "users" # between "name" (Closed $ str "a") (Open $ str "c") :: IO [JSON]
-- [{"post_count":2,"name":"bill","occupation":"pianist"}]
between :: (Expr left, Expr right, Expr seq) => Key -> Bound left -> Bound right -> seq -> ReQL
between i a b e =
  op' BETWEEN [expr e, expr $ getBound a, expr $ getBound b]
         ["left_bound" :== closedOrOpen a, "right_bound" :== closedOrOpen b, "index" :== i]

-- | Append a datum to a sequence
--
-- >>> run h $ append 3 [1, 2] :: IO (Maybe [Int])
-- Just [1,2,3]
append :: (Expr a, Expr b) => a -> b -> ReQL
append a b = op APPEND (b, a)

-- | Map a function of a sequence and concat the results
--
-- >>> run h $ concatMap id [[1, 2], [3], [4, 5]] :: IO (Maybe [Int])
-- Just [1,2,3,4,5]
concatMap :: (Expr a, Expr b) => (ReQL -> b) -> a -> ReQL
concatMap f e = op CONCATMAP (e, expr P.. f)

-- | SQL-like inner join of two sequences
--
-- >>> run' h $ innerJoin (\user post -> user!"name" R.== post!"author") (table "users") (table "posts") # mergeLeftRight # orderBy [Asc "id"] # pluck ["name", "message"]
-- [[{"name":"bill","message":"hi"},{"name":"bill","message":"hello"}]]
innerJoin :: (Expr a, Expr b, Expr c) => (ReQL -> ReQL -> c) -> a -> b -> ReQL
innerJoin f a b = op INNER_JOIN (a, b, fmap expr P.. f)

-- | SQL-like outer join of two sequences
--
-- >>> run' h $ outerJoin (\user post -> user!"name" R.== post!"author") (table "users") (table "posts") # mergeLeftRight # orderBy [Asc "id", Asc "name"] # pluck ["name", "message"]
-- [[{"name":"nancy"},{"name":"bill","message":"hi"},{"name":"bill","message":"hello"}]]
outerJoin :: (Expr a, Expr b, Expr c) => (ReQL -> ReQL -> c) -> a -> b -> ReQL
outerJoin f a b = op OUTER_JOIN (a, b, fmap expr P.. f)

-- | An efficient inner_join that uses a key for the left table and an index for the right table.
--
-- >>> run' h $ table "posts" # eqJoin "author" (table "users") "name" # mergeLeftRight # orderBy [Asc "id"] # pluck ["name", "message"]
-- [[{"name":"bill","message":"hi"},{"name":"bill","message":"hello"}]]
eqJoin :: (Expr right, Expr left) => Key -> right -> Key -> left -> ReQL
eqJoin key right index left = op' EQ_JOIN (left, key, right) ["index" :== index]

-- | Drop elements from the head of a sequence.
--
-- Called /skip/ in the official drivers
--
-- >>> run h $ R.drop 2 [1, 2, 3, 4] :: IO (Maybe [Int])
-- Just [3,4]
drop :: (Expr a, Expr b) => a -> b -> ReQL
drop a b = op SKIP (b, a)

-- | Limit the size of a sequence.
--
-- Called /limit/ in the official drivers
--
-- >>> run h $ R.take 2 [1, 2, 3, 4] :: IO (Maybe [Int])
-- Just [1,2]
take :: (Expr n, Expr seq) => n -> seq -> ReQL
take n s = op LIMIT (s, n)

-- | Cut out part of a sequence
--
-- >>> run h $ slice 2 4 [1, 2, 3, 4, 5] :: IO (Maybe [Int])
-- Just [3,4]
slice :: (Expr a, Expr b, Expr c) => a -> b -> c -> ReQL
slice n m s = op SLICE (s, n, m)

infixl 9 !!

-- | Get the nth value of a sequence or array
--
-- >>> run h $ [1, 2, 3] !! 0 :: IO (Maybe Int)
-- Just 1
(!!) :: (Expr a) => a -> ReQL -> ReQL
s !! n = op NTH (s, n)

-- | Reduce a sequence to a single value
--
-- >>> run h $ reduce (+) 0 [1, 2, 3] :: IO (Maybe Int)
-- Just 6
reduce :: (Expr base, Expr seq, Expr a) => (ReQL -> ReQL -> a) -> base -> seq -> ReQL
reduce f b s = op REDUCE (s ++ [b], fmap expr P.. f)

-- | Reduce a non-empty sequence to a single value
--
-- >>> run h $ reduce1 (+) [1, 2, 3] :: IO (Maybe Int)
-- Just 6
reduce1 :: (Expr a, Expr s) => (ReQL -> ReQL -> a) -> s -> ReQL
reduce1 f s = op REDUCE (s, fmap expr P.. f)

-- | Filter out identical elements of the sequence
--
-- Called /distint/ in the official drivers
--
-- >>> fmap (fmap sort) $ run h $ nub (table "posts" ! "flag") :: IO (Maybe [String])
-- Just ["deleted","pinned"]
nub :: (Expr s) => s -> ReQL
nub s = op DISTINCT [s]

-- | Merge the "left" and "right" attributes of the objects in a sequence.
--
-- Called /zip/ in the official drivers
--
-- >>> _ <- run' h $ table "posts" # eqJoin "author" (table "users") "name" # mergeLeftRight
mergeLeftRight :: (Expr a) => a -> ReQL
mergeLeftRight a = op ZIP [a]

-- | Ordering specification for orderBy
data Order =
  Asc { orderAttr :: Key } -- ^ Ascending order
  | Desc { orderAttr :: Key } -- ^ Descending order

-- | Order a sequence by the given keys
--
-- >>> run' h $ table "users" # orderBy [Desc "post_count", Asc "name"] # pluck ["name", "post_count"]
-- [[{"post_count":2,"name":"bill"},{"post_count":0.0,"name":"nancy"}]]
orderBy :: (Expr s) => [Order] -> s -> ReQL
orderBy o s = ReQL $ do
  s' <- runReQL (expr s)
  o' <- baseArray $ arr $ P.map buildOrder o
  return $ Term ORDERBY P.Nothing (s' : o') []
  where
    buildOrder (Asc k) = op ASC [k]
    buildOrder (Desc k) = op DESC [k]

-- TODO: orderBy index
-- TODO: orderBy function

-- | Turn a grouping function and a reduction function into a grouped map reduce operation
--
-- > >>> run' h $ table "posts" # groupBy (!"author") (reduce1 (\a b -> a + "\n" + b) . R.map (!"message"))
-- > [["hello\nhi","lorem ipsum"]]
-- >>> run' h $ table "users" # groupBy (!"level") (\users -> let pc = users!"post_count" in [average pc, R.sum pc])
-- [[{"group":1,"reduction":[1.5,3.0]},{"group":2,"reduction":[0.0,0.0]}]]
groupBy ::
  (Expr group, Expr reduction, Expr seq)
  => (ReQL -> group) -> (ReQL -> reduction) -> seq -> ReQL
groupBy g f s = ReQL $ do
  mr <- termToMapReduce (expr . f)
  let group = op GROUP (expr s, expr . g)
  runReQL $ op UNGROUP [mr group] ! "reduction"

-- | TODO
mapReduce :: (Expr reduction, Expr seq) => (ReQL -> reduction) -> seq -> ReQL
mapReduce f s = ReQL $ do
  mr <- termToMapReduce (expr . f)
  runReQL $ mr (expr s)

-- | The sum of a sequence
--
-- >>> run h $ sum [1, 2, 3] :: IO (Maybe Int)
-- Just 6
sum :: (Expr s) => s -> ReQL
sum s = op SUM [s]

-- | The average of a sequence
--
-- >>> run h $ average [1, 2, 3, 4] :: IO (Maybe Double)
-- Just 2.5
average :: (Expr s) => s -> ReQL
average s = op AVG [s]

-- | Minimum value
min :: Expr s => s -> ReQL
min s = op MIN [s]

-- | Value that minimizes the function
argmin :: (Expr s, Expr a) => (ReQL -> a) -> s -> ReQL
argmin f s = op MIN (s, expr . f)

-- | Minimum value
max :: Expr s => s -> ReQL
max s = op MAX [s]

-- | Value that maximizes the function
argmax :: (Expr s, Expr a) => (ReQL -> a) -> s -> ReQL
argmax f s = op MAX (s, expr . f)

-- * Accessors

infixl 9 !

-- | Get a single field from an object
--
-- >>> run h $ (obj ["foo" := True]) ! "foo" :: IO (Maybe Bool)
-- Just True
--
-- Or a single field from each object in a sequence
--
-- >>> run h $ [obj ["foo" := True], obj ["foo" := False]] ! "foo" :: IO (Maybe [Bool])
-- Just [True,False]
(!) :: (Expr s) => s -> ReQL -> ReQL
s ! k = op GET_FIELD (s, k)

-- | Get a single field, or null if not present
--
-- >>> run h $ obj [] !? "foo" :: IO (Maybe (Maybe Bool))
-- Just Nothing
(!?) :: (Expr s) => s -> ReQL -> ReQL
s !? k = op DEFAULT (op GET_FIELD (s, k), ())

-- | Keep only the given attributes
--
-- >>> run' h $ map obj [["a" := 1, "b" := 2], ["a" := 2, "c" := 7], ["b" := 4]] # pluck ["a"]
-- [[{"a":1},{"a":2},{}]]
pluck :: (Expr o) => [ReQL] -> o -> ReQL
pluck ks e = op PLUCK (cons e $ arr (P.map expr ks))

-- | Remove the given attributes from an object
--
-- >>> run' h $ map obj [["a" := 1, "b" := 2], ["a" := 2, "c" := 7], ["b" := 4]] # without ["a"]
-- [[{"b":2},{"c":7},{"b":4}]]
without :: (Expr o) => [ReQL] -> o -> ReQL
without ks e = op WITHOUT (cons e $ arr (P.map expr ks))

-- | Test if a sequence contains a given element
--
-- >>> run' h $ 1 `R.elem` [1,2,3]
-- [true]
elem :: (Expr x, Expr seq) => x -> seq -> ReQL
elem x s = op CONTAINS (s, x)

-- | Merge two objects together
--
-- >>> run' h $ merge (obj ["a" := 1, "b" := 1]) (obj ["b" := 1, "c" := 2])
-- [{"a":1,"b":1,"c":2}]
merge :: (Expr a, Expr b) => a -> b -> ReQL
merge a b = op MERGE (b, a)

-- | Literal objects, in a merge or update, are not processed recursively.
--
-- >>> run' h $ (obj ["a" := obj ["b" := 1]]) # merge (obj ["a" := literal (obj ["c" := 2])])
-- [{"a":{"c":2}}]
literal :: Expr a => a -> ReQL
literal a = op LITERAL [a]

-- | Remove fields when doing a merge or update
--
-- >>> run' h $ obj ["a" := obj ["b" := 1]] # merge (obj ["a" := remove])
-- [{}]
remove :: ReQL
remove = op LITERAL ()

-- | Evaluate a JavaScript expression
--
-- >>> _ <- run h $ js "Math.random()" :: IO (Maybe Double)
-- >>> run h $ R.map (\x -> js "Math.sin" `apply` [x]) [pi, pi/2] :: IO (Maybe [Double])
-- Just [1.2246063538223773e-16,1.0]
js :: ReQL -> ReQL
js s = op JAVASCRIPT [s]

-- | Called /branch/ in the official drivers
--
-- >>> run h $ if' (1 R.< 2) 3 4 :: IO (Maybe Int)
-- Just 3
if' :: (Expr a, Expr b, Expr c) => a -> b -> c -> ReQL
if' a b c = op BRANCH (a, b, c)

-- | Abort the query with an error
--
-- >>> run' h $ R.error (str "haha") R./ 2 + 1
-- *** Exception: RethinkDBError {errorCode = runtime error, errorTerm = ADD(DIV(ERROR("haha"), 2.0), 1.0), errorMessage = "haha", errorBacktrace = [0,0]}
error :: (Expr s) => s -> ReQL
error m = op ERROR [m]

-- | Create a Database reference
--
-- >>> run' h $ db "test" # info
-- [{"name":"test","type":"DB"}]
db :: Text -> O.Database
db s = O.Database s

-- | Create a database on the server
--
-- >>> run' h $ dbCreate "dev"
-- [{"created":1}]
dbCreate :: P.String -> ReQL
dbCreate db_name = op DB_CREATE [str db_name]

-- | Drop a database
--
-- >>> run' h $ dbDrop (db "dev")
-- [{"dropped":1}]
dbDrop :: Database -> ReQL
dbDrop (O.Database name) = op DB_DROP [name]

-- | List the databases on the server
--
-- >>> _ <- run h $ dbList :: IO (Maybe [String])
dbList :: ReQL
dbList = op DB_LIST ()

-- | Create an index on the table from the given function
--
-- >>> run' h $ table "users" # indexCreate "occupation" (!"occupation") def
-- [{"created":1}]
indexCreate :: (Expr fun) => P.String -> fun -> IndexCreateOptions -> Table -> ReQL
indexCreate name f opts tbl = op' INDEX_CREATE (tbl, str name, f) $ catMaybes [
  ("multi" :==) <$> indexMulti opts]

-- | List the indexes on the table
--
-- >>> run' h $ indexList (table "users")
-- [["occupation"]]
indexList :: Table -> ReQL
indexList tbl = op INDEX_LIST [tbl]

-- | Drop an index
--
-- >>> run' h $ table "users" # indexDrop "occupation"
-- [{"dropped":1}]
indexDrop :: Key -> Table -> ReQL
indexDrop name tbl = op INDEX_DROP (tbl, name)

-- | Retreive documents by their indexed value
--
-- >>> run' h $ table "users" # getAll "name" [str "bill"]
-- [[{"post_count":2,"name":"bill","occupation":"pianist"}]]
getAll :: (Expr value) => Key -> [value] -> Table -> ReQL
getAll idx xs tbl = op' GET_ALL (expr tbl : P.map expr xs) ["index" :== idx]

-- | Get a document by primary key
--
-- >>> run' h $ table "users" # get "nancy"
-- [{"post_count":0.0,"name":"nancy"}]
get :: Expr s => ReQL -> s -> ReQL
get k e = op GET (e, k)

-- | Convert a value to a different type
--
-- >>> run h $ coerceTo "STRING" 1 :: IO (Maybe String)
-- Just "1"
coerceTo :: (Expr x) => ReQL -> x -> ReQL
coerceTo t a = op COERCE_TO (a, t)

-- | Convert a value to an array
--
-- >>> run h $ asArray $ obj ["a" := 1, "b" := 2] :: IO (Maybe [(String, Int)])
-- Just [("a",1),("b",2)]
asArray :: Expr x => x -> ReQL
asArray = coerceTo "ARRAY"

-- | Convert a value to a string
--
-- >>> run h $ asString $ obj ["a" := 1, "b" := 2] :: IO (Maybe String)
-- Just "{\n\t\"a\":\t1,\n\t\"b\":\t2\n}"
asString :: Expr x => x -> ReQL
asString = coerceTo "STRING"

-- | Convert a value to a number
--
-- >>> run h $ asNumber (str "34") :: IO (Maybe Int)
-- Just 34
asNumber :: Expr x => x -> ReQL
asNumber = coerceTo "NUMBER"

-- | Convert a value to an object
--
-- >>> run' h $ asObject $ [(str "a",1),("b",2)]
-- [{"a":1,"b":2}]
asObject :: Expr x => x -> ReQL
asObject = coerceTo "OBJECT"

-- | Convert a value to a boolean
asBool :: Expr x => x -> ReQL
asBool = coerceTo "BOOL"

-- | Like hasFields followed by pluck
--
-- >>> run' h $ map obj [["a" := 1, "b" := 2], ["a" := 2, "c" := 7], ["b" := 4]] # withFields ["a"]
-- [[{"a":1},{"a":2}]]
withFields :: Expr seq => [ReQL] -> seq -> ReQL
withFields p s = op WITH_FIELDS (s, p)

-- | The position in the sequence of the elements that match the predicate
--
-- >>> run h $ indexesOf (=~ "ba.") [str "foo", "bar", "baz"] :: IO (Maybe [Int])
-- Just [1,2]
indexesOf :: (Expr fun, Expr seq) => fun -> seq -> ReQL
indexesOf f s = op INDEXES_OF (s, f)

-- | Test if a sequence is empty
--
-- >>> run h $ isEmpty [1] :: IO (Maybe Bool)
-- Just False
isEmpty :: Expr seq => seq -> ReQL
isEmpty s = op IS_EMPTY [s]

-- | Select a given number of elements from a sequence with uniform random distribution
--
-- >>> _ <- run h $ sample 3 [0,1,2,3,4,5,6,7,8,9] :: IO (Maybe [Int])
sample :: (Expr n, Expr seq) => n -> seq -> ReQL
sample n s = op SAMPLE (s, n)

-- | Prepend an element to an array
--
-- >>> run h $ prepend 1 [2,3] :: IO (Maybe [Int])
-- Just [1,2,3]
prepend :: (Expr datum, Expr array) => datum -> array -> ReQL
prepend d a = op PREPEND (a, d)

infixl 9 \\ --

-- | Called /difference/ in the official drivers
--
-- >>> run h $ [1,2,3,4,5] \\ [2,5] :: IO (Maybe [Int])
-- Just [1,3,4]
(\\) :: (Expr a, Expr b) => a -> b -> ReQL
a \\ b = op DIFFERENCE (a, b)

-- | Insert a datum into an array if it is not yet present
--
-- >>> run h $ setInsert 3 [1,2,4,4,5] :: IO (Maybe [Int])
-- Just [1,2,4,5,3]
setInsert :: (Expr datum, Expr array) => datum -> array -> ReQL
setInsert d a = op SET_INSERT (a, d)

-- | The union of two sets
--
-- >>> run h $ [1,2] `setUnion` [2,3]  :: IO (Maybe [Int])
-- Just [2,3,1]
setUnion :: (Expr a, Expr b) => a -> b -> ReQL
setUnion a b = op SET_UNION (b, a)

-- | The intersection of two sets
--
-- >>> run h $ [1,2] `setIntersection` [2,3]  :: IO (Maybe [Int])
-- Just [2]
setIntersection :: (Expr a, Expr b) => a -> b -> ReQL
setIntersection a b = op SET_INTERSECTION (b, a)

-- | The difference of two sets
--
-- >>> run h $ [2,3] # setDifference [1,2]  :: IO (Maybe [Int])
-- Just [3]
setDifference :: (Expr set, Expr remove) => remove -> set -> ReQL
setDifference r s = op SET_DIFFERENCE (s, r)

-- | Test if an object has the given fields
--
-- >>> run h $ hasFields "a" $ obj ["a" := 1] :: IO (Maybe Bool)
-- Just True
hasFields :: (Expr obj) => ReQL -> obj -> ReQL
hasFields p o = op HAS_FIELDS (o, expr p)

-- | Insert a datum at the given position in an array
--
-- >>> run h $ insertAt 1 4 [1,2,3] :: IO (Maybe [Int])
-- Just [1,4,2,3]
insertAt :: (Expr n, Expr datum, Expr array) => n -> datum -> array -> ReQL
insertAt n d a = op INSERT_AT (a, n, d)

-- | Splice an array at a given position inside another array
--
-- >>> run h $ spliceAt 2 [4,5] [1,2,3] :: IO (Maybe [Int])
-- Just [1,2,4,5,3]
spliceAt :: (Expr n, Expr replace, Expr array) => n -> replace -> array -> ReQL
spliceAt n s a = op SPLICE_AT (a, n, s)

-- | Delete an element from an array
--
-- >>> run h $ deleteAt 1 [1,2,3] :: IO (Maybe [Int])
-- Just [1,3]
deleteAt :: (Expr n, Expr array) => n -> array -> ReQL
deleteAt n a = op DELETE_AT (a, n)

-- | Change an element in an array
--
-- >>> run h $ changeAt 1 4 [1,2,3] :: IO (Maybe [Int])
-- Just [1,4,3]
changeAt :: (Expr n, Expr datum, Expr array) => n -> datum -> array -> ReQL
changeAt n d a = op CHANGE_AT (a, n, d)

-- | The list of keys of the given object
--
-- >>> run h $ keys (obj ["a" := 1, "b" := 2]) :: IO (Maybe [String])
-- Just ["a","b"]
keys :: Expr obj => obj -> ReQL
keys o = op KEYS [o]

-- | Match a string to a regular expression.
--
-- Called /match/ in the official drivers
--
-- >>> run' h $ str "foobar" =~ "f(.)+[bc](.+)"
-- [{"groups":[{"start":2,"end":3,"str":"o"},{"start":4,"end":6,"str":"ar"}],"start":0.0,"end":6,"str":"foobar"}]
(=~) :: (Expr string) => string -> ReQL -> ReQL
s =~ r = op MATCH (s, r)

-- | Apply a function to a list of arguments.
--
-- Called /do/ in the official drivers
--
-- >>> run h $ (\x -> x R.* 2) `apply` [4] :: IO (Maybe Int)
-- Just 8
apply :: (Expr fun, Expr arg) => fun -> [arg] -> ReQL
f `apply` as = op FUNCALL (expr f : P.map expr as)

-- | Catch some expections inside the query.
--
-- Called /default/ in the official drivers
--
-- >>> run h $ R.handle 0 $ obj ["a" := 1] ! "b" :: IO (Maybe Int)
-- Just 0
-- >>> run h $ R.handle (expr . id) $ obj ["a" := 1] ! "b" :: IO (Maybe String)
-- Just "No attribute `b` in object:\n{\n\t\"a\":\t1\n}"
handle :: (Expr instead, Expr reql) => (ReQL -> instead) -> reql -> ReQL
handle h r = op DEFAULT (r, expr . h)

-- | A string representing the type of an expression
--
-- >>> run h $ typeOf 1 :: IO (Maybe String)
-- Just "NUMBER"
typeOf :: Expr a => a -> ReQL
typeOf a = op TYPEOF [a]

-- | Get information on a given expression. Useful for tables and databases.
--
-- >>> run' h $ info $ table "users"
-- [{"primary_key":"name","name":"users","indexes":[],"type":"TABLE","db":{"name":"doctests","type":"DB"}}]
info :: Expr a => a -> ReQL
info a = op INFO [a]

-- | Parse a json string into an object
--
-- >>> run' h $ json "{\"a\":1}"
-- [{"a":1}]
json :: ReQL -> ReQL
json s = op JSON [s]

-- | Flipped function application
infixl 8 #
(#) :: (Expr a, Expr b) =>  a -> (a -> b) -> ReQL
x # f = expr (f x)

infixr 9 .
-- | Specialised function composition
(.) :: (Expr a, Expr b, Expr c) =>  (ReQL -> b) -> (ReQL -> a) -> c -> ReQL
(f . g) x = expr (f (expr (g (expr x))))

-- | Case manipulation
--
-- >>> run h $ toUpper (str "Foo") :: IO (Maybe String)
-- Just "FOO"
toUpper, toLower :: Expr str => str -> ReQL
toUpper s = op UPCASE [s]
toLower s = op DOWNCASE [s]

-- | Split a string on whitespace characters
--
-- >>> run' h $ split (str "foo bar")
-- [["foo","bar"]]
split :: Expr str => str -> ReQL
split s = op SPLIT [s]

-- | Split a string on a given delimiter
--
-- >>> run' h $ str "foo, bar" # splitOn ","
-- [["foo"," bar"]]
splitOn :: Expr str => ReQL -> str -> ReQL
splitOn sep s = op SPLIT [expr s, sep]

-- | Split a string up to a given number of times
--
-- >>> run' h $ str "a:b:c:d" # splitMax ":" 2
-- [["a","b","c:d"]]
splitMax :: Expr str => ReQL -> ReQL -> str -> ReQL
splitMax sep n s = op SPLIT [expr s, sep, n]

-- TODO: index_status, index_wait, sync