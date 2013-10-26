{-# LANGUAGE FlexibleInstances, OverloadedStrings, GADTs #-}

-- TODO: operator fixity

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

infixl 6 +, -
infixl 7 *, /

-- | Addition or concatenation
--
-- Use the Num instance, or a qualified operator.
--
-- > run h $ 2 + 5 :: IO (Maybe Int)
-- > run h $ 2 R.+ 5 :: IO (Maybe Int)
--
-- > run h $ (str "foo") + (str "bar") :: IO (Just String)
(+) :: (Expr a, Expr b) => a -> b -> ReQL
(+) a b = op ADD (a, b) ()

-- | Subtraction
--
-- > run h $ 2 - 5 :: IO (Maybe Int)
(-) :: (Expr a, Expr b) => a -> b -> ReQL
(-) a b = op SUB (a, b) ()

-- | Multiplication
--
-- > run h $ 2 * 5 :: IO (Maybe Int)
(*) :: (Expr a, Expr b) => a -> b -> ReQL
(*) a b = op MUL (a, b) ()

-- | Division
--
-- If you ask for an Int instead of a Double, Aeson will round the result
--
-- > run h $ 2 / 5 :: IO (Maybe Double)
(/) :: (Expr a, Expr b) => a -> b -> ReQL
(/) a b = op DIV (a, b) ()

-- | Mod
--
-- > run h $ 5 `mod` 2 :: IO (Maybe Int)
mod :: (Expr a, Expr b) => a -> b -> ReQL
mod a b = op MOD (a, b) ()

infixr 2 ||
infixr 3 &&

-- | Boolean or
--
-- > run h $ True R.|| False :: IO (Maybe Bool)
(||) :: (Expr a, Expr b) => a -> b -> ReQL
a || b = op ANY (a, b) ()

-- | Boolean and
--
-- > run h $ True R.&& False :: IO (Maybe Bool)
(&&) :: (Expr a, Expr b) => a -> b -> ReQL
a && b = op ALL (a, b) ()

infix 4 ==, /=

-- | Test for equality
--
-- > run h $ obj ["a" := 1] R.== obj ["a" := 1] :: IO (Maybe Bool)
(==) :: (Expr a, Expr b) => a -> b -> ReQL
a == b = op EQ (a, b) ()

-- | Test for inequality
--
-- > run h $ 1 R./= False :: IO (Maybe Bool)
(/=) :: (Expr a, Expr b) => a -> b -> ReQL
a /= b = op NE (a, b) ()

infix 4 >, <, <=, >=

-- | Greater than
--
-- > run h $ 3 R.> 2 :: IO (Maybe Bool)
(>) :: (Expr a, Expr b) => a -> b -> ReQL
a > b = op GT (a, b) ()

-- | Lesser than
--
-- > run h $ (str "a") R.< (str "b") :: IO (Maybe Bool)
(<) :: (Expr a, Expr b) => a -> b -> ReQL
a < b = op LT (a, b) ()

-- | Greater than or equal to
--
-- > run h $ [1] R.>= () :: IO (Maybe Bool)
(>=) :: (Expr a, Expr b) => a -> b -> ReQL
a >= b = op GE (a, b) ()

-- | Lesser than or equal to
--
-- > run h $ 2 R.<= 2 :: IO (Maybe Bool)
(<=) :: (Expr a, Expr b) => a -> b -> ReQL
a <= b = op LE (a, b) ()

-- | Negation
--
-- > run h $ R.not False :: IO (Maybe Bool)
-- > run h $ R.not () :: IO (Maybe Bool)
not :: (Expr a) => a -> ReQL
not a = op NOT [a] ()

-- * Lists and Streams

-- | The size of a sequence or an array.
--
-- Called /count/ in the official drivers
--
-- > run h $ R.length (table "foo") :: IO (Maybe Int)
length :: (Expr a) => a -> ReQL
length e = op COUNT [e] ()

infixr 5 ++

-- | Join two sequences.
--
-- Called /union/ in the official drivers
--
-- Use (R.+) for concatenating strings
--
-- > run h $ [1,2,3] R.++ ["a", "b", "c" :: Text] :: IO (Maybe [Value])
(++) :: (Expr a, Expr b) => a -> b -> ReQL
a ++ b = op UNION (a, b) ()

-- | Map a function over a sequence
--
-- > run h $ R.map (!"a") [obj ["a" := 1], obj ["a" := 2]] :: IO (Maybe [String])
map :: (Expr a, Expr b) => (ReQL -> b) -> a -> ReQL
map f a = op MAP (a, expr P.. f) ()

-- | Filter a sequence given a predicate
--
-- > run h $ R.filter (R.< 4) [3, 1, 4, 1, 5, 9, 2, 6] :: IO (Maybe [Int])
filter :: (Expr predicate, Expr seq) => predicate -> seq -> ReQL
filter f a = op FILTER (a, f) ()

-- | Query all the documents whose value for the given index is in a given range
--
-- > run h $ table "foo" # between "id" (Closed $ str "m") (Open $ str "n") :: IO [Value])
between :: (Expr left, Expr right, Expr seq) => Key -> Bound left -> Bound right -> seq -> ReQL
between i a b e =
  op BETWEEN [expr e, expr $ getBound a, expr $ getBound b]
         ["left_bound" := closedOrOpen a, "right_bound" := closedOrOpen b, "index" := i]

-- | Append a datum to a sequence
--
-- > run h $ append 3 [1, 2] :: IO (Maybe [Int])
append :: (Expr a, Expr b) => a -> b -> ReQL
append a b = op APPEND (b, a) ()

-- | Map a function of a sequence and concat the results
--
-- > run h $ concatMap id [[1, 2], [3], [4, 5]] :: IO (Maybe [Int])
concatMap :: (Expr a, Expr b) => (ReQL -> b) -> a -> ReQL
concatMap f e = op CONCATMAP (e, expr P.. f) ()

-- | SQL-like inner join of two sequences
--
-- > run' h $ tableCreate (table "posts") def
-- > run' h $ tableCreate (table "users") def
-- > Just wr@WriteResponse{} <- run h $ table "users" # insert (map (\x -> obj ["name":=x]) ["bill", "bob", "nancy" :: Text])
-- > let Just [bill, bob, nancy] = writeResponseGeneratedKeys wr
-- > run' h $ table "posts" # insert (obj ["author" := bill, "message" := str "hi"])
-- > run' h $ table "posts" # insert (obj ["author" := bill, "message" := str "hello"])
-- > run' h $ table "posts" # insert (obj ["author" := bob, "message" := str "lorem ipsum"])
-- > run' h $ innerJoin (\user post -> user!"id" R.== post!"author") (table "users") (table "posts") # mergeLeftRight # without ["id", "author"]
innerJoin :: (Expr a, Expr b, Expr c) => (ReQL -> ReQL -> c) -> a -> b -> ReQL
innerJoin f a b = op INNER_JOIN (a, b, fmap expr P.. f) ()

-- | SQL-like outer join of two sequences
--
-- > run' h $ outerJoin (\user post -> user!"id" R.== post!"author") (table "users") (table "posts")
outerJoin :: (Expr a, Expr b, Expr c) => (ReQL -> ReQL -> c) -> a -> b -> ReQL
outerJoin f a b = op OUTER_JOIN (a, b, fmap expr P.. f) ()

-- | An efficient inner_join that uses a key for the left table and an index for the right table.
--
-- > run' h $ table "posts" # eqJoin "author" (table "users") "id"
eqJoin :: (Expr right, Expr left) => Key -> right -> Key -> left -> ReQL
eqJoin key right index left = op EQ_JOIN (left, key, right) ["index" := index]

-- | Drop elements from the head of a sequence.
--
-- Called /skip/ in the official drivers
--
-- > run h $ R.drop 2 [1, 2, 3, 4] :: IO (Maybe [Int])
drop :: (Expr a, Expr b) => a -> b -> ReQL
drop a b = op SKIP (b, a) ()

-- | Limit the size of a sequence.
--
-- Called /limit/ in the official drivers
--
-- > run h $ R.take 2 [1, 2, 3, 4] :: IO (Maybe [Int])
take :: (Expr n, Expr seq) => n -> seq -> ReQL
take n s = op LIMIT (s, n) ()

-- | Cut out part of a sequence
--
-- > run h $ slice 2 4 [1, 2, 3, 4, 5] :: IO (Maybe [Int])
slice :: (Expr a, Expr b, Expr c) => a -> b -> c -> ReQL
slice n m s = op SLICE (s, n, m) ()

infixl 9 !!

-- | Get the nth value of a sequence or array
--
-- > run h $ [1, 2, 3] !! 0 :: IO (Maybe Int)
(!!) :: (Expr a) => a -> ReQL -> ReQL
s !! n = op NTH (s, n) ()

-- | Reduce a sequence to a single value
--
-- > run h $ reduce (+) 0 [1, 2, 3] :: IO (Maybe Int)
reduce :: (Expr base, Expr seq, Expr a) => (ReQL -> ReQL -> a) -> base -> seq -> ReQL
reduce f b s = op REDUCE (s, fmap expr P.. f) ["base" := b]

-- | Reduce a non-empty sequence to a single value
--
-- > run h $ reduce1 (+) [1, 2, 3] :: IO (Maybe Int)
reduce1 :: (Expr a, Expr s) => (ReQL -> ReQL -> a) -> s -> ReQL
reduce1 f s = op REDUCE (s, fmap expr P.. f) ()

-- | Filter out identical elements of the sequence
--
-- Called /distint/ in the official drivers
--
-- > run h $ nub (table "foo") :: IO (Just [Value])
nub :: (Expr s) => s -> ReQL
nub s = op DISTINCT [s] ()

-- | Like map but for write queries
--
-- > run' h $ table "users" # replace (without ["post_count"])
-- > run' h $ forEach (table "posts") $ \post -> table "users" # get (post!"author") # update (\user -> obj ["post_count" := (handle 0 (user!"post_count") + 1)])
forEach :: (Expr s, Expr a) => s -> (ReQL -> a) -> ReQL
forEach s f = op FOREACH (s, expr P.. f) ()

-- | Merge the "left" and "right" attributes of the objects in a sequence.
--
-- Called /zip/ in the official drivers
--
-- > run' h $ table "posts" # eqJoin "author" (table "users") "id" # mergeLeftRight
mergeLeftRight :: (Expr a) => a -> ReQL
mergeLeftRight a = op ZIP [a] ()

-- | Ordering specification for orderBy
data Order =
  Asc { orderAttr :: Key } -- ^ Ascending order
  | Desc { orderAttr :: Key } -- ^ Descending order

-- | Order a sequence by the given keys
--
-- run' h $ table "users" # orderBy [Desc "post_count", Asc "name"]
orderBy :: (Expr s) => [Order] -> s -> ReQL
orderBy o s = ReQL $ do
  s' <- baseReQL (expr s)
  o' <- baseArray $ arr $ P.map buildOrder o
  return $ BaseReQL ORDERBY P.Nothing (s' : o') []
  where
    buildOrder (Asc k) = op ASC [k] ()
    buildOrder (Desc k) = op DESC [k] ()

-- | Turn a grouping function and a reduction function into a grouped map reduce operation
--
-- > run' h $ table "posts" # groupBy (!"author") (reduce1 (\a b -> a + "\n" + b) . R.map (!"message"))
-- > run' h $ table "users" # groupBy (!"level") (\users -> let pc = users!"post_count" in [avg pc, R.sum pc])
groupBy ::
  (Expr group, Expr reduction, Expr seq)
  => (ReQL -> group) -> (ReQL -> reduction) -> seq -> ReQL
groupBy g mr s = ReQL $ do
  (m, r, mf) <- termToMapReduce (expr . mr)
  let gmr = op GROUPED_MAP_REDUCE [expr s, expr $ expr P.. g, expr m, expr r] ()
  baseReQL $ case mf of
    Nothing -> gmr
    Just f -> op MAP [gmr, expr $ \x -> expr $
                      obj ["group" := (x!"group"), "reduction" := f (x!"reduction")]] ()

-- | The sum of a sequence
--
-- > run h $ sum [1, 2, 3] :: IO (Maybe Int)
sum :: (Expr s) => s -> ReQL
sum = reduce ((+) :: ReQL -> ReQL -> ReQL) (num 0)

-- | The average of a sequence
--
-- > run h $ avg [1, 2, 3, 4] :: IO (Maybe Double)
avg :: (Expr s) => s -> ReQL
avg = (\x -> (x!!0) / (x!!1)) .
  reduce (\a b -> [(a!!0) + (b!!0), (a!!1) + (b!!1)]) [num 0, num 0] .
  map (\x -> [x, 1])

-- * Accessors

infixl 9 !

-- | Get a single field from an object
--
-- > run h $ (obj ["foo" := True]) ! "foo" :: IO (Maybe Bool)
--
-- Or a single field from each object in a sequence
--
-- > run h $ [obj ["foo" := True], obj ["foo" := False]] ! "foo" :: IO (Maybe [Bool])
(!) :: (Expr s) => s -> ReQL -> ReQL
s ! k = op GET_FIELD (s, k) ()

-- | Keep only the given attributes
pluck :: (Expr o) => [ReQL] -> o -> ReQL
pluck ks e = op PLUCK (cons e $ arr (P.map expr ks)) ()

-- | Remove the given attributes from an object
without :: (Expr o) => [ReQL] -> o -> ReQL
without ks e = op WITHOUT (cons e $ arr (P.map expr ks)) ()

-- | Test if an object contains the given attribute.
-- Called /contains/ in the official drivers
member :: (Expr o) => [ReQL] -> o -> ReQL
member ks o = op CONTAINS (cons o $ arr (P.map expr ks)) ()

-- | Merge two objects together
merge :: (Expr a, Expr b) => a -> b -> ReQL
merge a b = op MERGE (a, b) ()

-- | Create a javascript expression
class Javascript r where
  js :: P.String -> r

instance Javascript ReQL where
  js s = op JAVASCRIPT [str s] ()

instance a ~ ReQL => Javascript (a -> ReQL) where
  js s x = op FUNCALL (op JAVASCRIPT [str s] (), x) ()

instance (a ~ ReQL, b ~ ReQL) => Javascript (a -> b -> ReQL) where
  js s x y = op FUNCALL (op JAVASCRIPT [str s] (), x, y) ()

-- | Called /branch/ in the official drivers
if' :: (Expr a, Expr b, Expr c) => a -> b -> c -> ReQL
if' a b c = op BRANCH (a, b, c) ()

-- | Abort the query with an error
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
dbList :: ReQL
dbList = op DB_LIST () ()

-- | Create an index on the table from the given function
indexCreate :: (Expr fun) => P.String -> fun -> Table -> ReQL
indexCreate name f tbl = op INDEX_CREATE (tbl, str name, f) ()

-- | Drop an index
indexDrop :: Key -> Table -> ReQL
indexDrop name tbl = op INDEX_DROP (tbl, name) ()

-- | List the indexes on the table
indexList :: Table -> ReQL
indexList tbl = op INDEX_LIST [tbl] ()

-- | Retreive documents by their indexed value
getAll :: (Expr value) => Key -> [value] -> Table -> ReQL
getAll idx xs tbl = op GET_ALL (expr tbl : P.map expr xs) ["index" := idx]

-- | Create a simple table refence with no associated database
table :: Text -> Table
table n = O.Table Nothing n Nothing

-- | Create a table on the server
--
-- > run' h $ tableCreate (table "foo") def
-- > run' h $ tableCreate (Table (db "prod") "bar" (Just "name")) def{ tableDataCenter = Just "cloud", tableCacheSize = Just 10 }
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
--
-- > run h $ tableList (db "test") :: IO [[String]]
tableList :: Database -> ReQL
tableList name = op TABLE_LIST [name] ()

-- | Get a document by primary key
get :: (Expr s, Expr k) => k -> s -> ReQL
get k e = op GET (e, k) ()

-- | Insert a document or a list of documents into a table
insert :: (Expr object) => object -> Table -> ReQL
insert a tb = canReturnVals $ op INSERT (tb, a) ()

-- | Like insert, but update existing documents with the same primary key
upsert :: (Expr table, Expr object) => object -> table -> ReQL
upsert a tb = canReturnVals $ op INSERT (tb, a) ["upsert" := P.True]

-- | Add to or modify the contents of a document
update :: (Expr selection, Expr a) => (ReQL -> a) -> selection -> ReQL
update f s = canNonAtomic $ canReturnVals $ op UPDATE (s, expr . f) ()

-- | Replace a document with another
replace :: (Expr selection, Expr a) => (ReQL -> a) -> selection -> ReQL
replace f s = canNonAtomic $ canReturnVals $ op REPLACE (s, expr . f) ()

-- | Delete the documents
delete :: (Expr selection) => selection -> ReQL
delete s = canReturnVals $ op DELETE [s] ()

-- | Convert a value to a different type
coerceTo :: (Expr x) => ReQL -> x -> ReQL
coerceTo t a = op COERCE_TO (a, t) ()

-- | Convert a value to a different type
asArray, asString, asNumber, asObject, asBool :: Expr x => x -> ReQL
asArray = coerceTo "ARRAY"
asString = coerceTo "STRING"
asNumber = coerceTo "NUMBER"
asObject = coerceTo "OBJECT"
asBool = coerceTo "BOOL"

-- | Like hasFields followed by pluck
withFields :: (Expr paths, Expr seq) => [paths] -> seq -> ReQL
withFields p s = op WITH_FIELDS (s, p) ()

-- | The position in the sequence of the elements that match the predicate
indexesOf :: (Expr fun, Expr seq) => fun -> seq -> ReQL
indexesOf f s = op INDEXES_OF (s, f) ()

-- | Test if a sequence is empty
isEmpty :: Expr seq => seq -> ReQL
isEmpty s = op IS_EMPTY [s] ()

-- | Select a given number of elements from a sequence with uniform random distribution
sample :: (Expr n, Expr seq) => n -> seq -> ReQL
sample n s = op SAMPLE (s, n) ()

-- | Prepend an element to an array
prepend :: (Expr datum, Expr array) => datum -> array -> ReQL
prepend d a = op PREPEND (a, d) ()

-- | Called /difference/ in the official drivers
infixl 9 \\ --
(\\) :: (Expr a, Expr b) => a -> b -> ReQL
a \\ b = op DIFFERENCE (a, b) ()

-- | Insert a datum into an array if it is not yet present
setInsert :: (Expr datum, Expr array) => datum -> array -> ReQL
setInsert d a = op SET_INSERT (a, d) ()

-- | The union of two sets
setUnion :: (Expr a, Expr b) => a -> b -> ReQL
setUnion a b = op SET_UNION (b, a) ()

-- | The intersection of two sets
setIntersection :: (Expr a, Expr b) => a -> b -> ReQL
setIntersection a b = op SET_INTERSECTION (b, a) ()

-- | The difference of two sets
setDifference :: (Expr set, Expr remove) => remove -> set -> ReQL
setDifference r s = op SET_DIFFERENCE (s, r) ()

-- | Test if an object has the given fields
hasFields :: (Expr obj, Expr paths) => paths -> obj -> ReQL
hasFields p o = op HAS_FIELDS (o, p) ()

-- | Insert a datum at the given position in an array
insertAt :: (Expr n, Expr datum, Expr array) => n -> datum -> array -> ReQL
insertAt n d a = op INSERT_AT (a, n, d) ()

-- | Splice an array at a given position inside another array
spliceAt :: (Expr n, Expr replace, Expr array) => n -> replace -> array -> ReQL
spliceAt n s a = op SPLICE_AT (a, n, s) ()

-- | Delete an element from an array
deleteAt :: (Expr n, Expr array) => n -> array -> ReQL
deleteAt n a = op DELETE_AT (a, n) ()

-- | Change an element in an array
changeAt :: (Expr n, Expr datum, Expr array) => n -> datum -> array -> ReQL
changeAt n d a = op CHANGE_AT (a, n, d) ()

-- | The list of keys of the given object
keys :: Expr obj => obj -> ReQL
keys o = op KEYS [o] ()

-- | Match a string to a regular expression.
-- Called /match/ in the official drivers
(=~) :: (Expr string) => string -> ReQL -> ReQL
s =~ r = op MATCH (s, r) ()

-- | Apply a function to a list of arguments.
-- Called /do/ in the official drivers
apply :: (Expr fun, Expr arg) => fun -> [arg] -> ReQL
f `apply` as = op FUNCALL (expr f : P.map expr as) ()

-- | Catch some expections inside the query.
-- Called /default/ in the official drivers
handle :: (Expr handler, Expr reql) => handler -> reql -> ReQL
handle h r = op DEFAULT (r, h) ()

-- | A string representing the type of an expression
typeOf :: Expr a => a -> ReQL
typeOf a = op TYPEOF [a] ()

-- | Get information on a given expression. Useful for tables and databases.
info :: Expr a => a -> ReQL
info a = op INFO [a] ()

-- | Parse a json string into an object
json :: Expr string => string -> ReQL
json s = op JSON [s] ()

-- | Flipped function application
infixl 8 #
(#) :: (Expr a, Expr b) =>  a -> (a -> b) -> ReQL
x # f = expr (f x)

infixr 9 .
-- | Specialised function composition
(.) :: (Expr a, Expr b, Expr c) =>  (ReQL -> b) -> (ReQL -> a) -> c -> ReQL
(f . g) x = expr (f (expr (g (expr x))))