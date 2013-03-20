{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds, FlexibleContexts, PolyKinds, 
             OverloadedStrings, FlexibleInstances #-}

-- | Functions from the ReQL (RethinkDB Query Language)

module Database.RethinkDB.Functions where

-- TODO: replace Term Sequence with Term Stream or Term Array

import Database.RethinkDB.Term
import Database.RethinkDB.Objects as O
import Database.RethinkDB.Type as T

import Database.RethinkDB.Protobuf.Ql2.Term2.TermType

import Prelude (($), return)
import qualified Prelude as P

(+), add, (-), sub, (*), mul, (/), div, div', mod, mod'
  :: (a ~~ Number, b ~~ Number) => a -> b -> Term Number
(+) a b = op ADD (a, b) []
(-) a b = op SUB (a, b) []
(*) a b = op MUL (a, b) []
(/) a b = op DIV (a, b) []
add = (+)
sub = (-)
mul = (*)
div = (/)
div' = (/)
mod a b = op MOD (a, b) []
mod' = mod

or, or', and, and' :: (a ~~ T.Bool, b ~~ T.Bool) => a -> b -> Term Bool
or a b = op ANY (a, b) []
and a b = op ALL (a, b) []
or' = or
and' = and

(==), (!=), eq, ne :: (a ~~ Datum, b ~~ Datum) => a -> b -> Term Bool
eq a b = op EQ (a, b) []
ne a b = op NE (a, b) []
(==) = eq
(!=) = ne

(>), (>=), (<), (<=), gt, lt, ge, le
  :: (a ~~ Datum, b ~~ Datum) => a -> b -> Term Bool
gt a b = op GT (a, b) []
lt a b = op LT (a, b) []
ge a b = op GE (a, b) []
le a b = op LE (a, b) []
(>) = gt
(>=) = ge
(<) = lt
(<=) = le

not, not' :: (a ~~ Bool) => a -> Term Bool
not a = op NOT [a] []
not' = not

-- * Lists and Streams

count :: (a ~~ Sequence) => a -> Term Number
count e = op COUNT [e] []

(++), concat :: (a ~~ Sequence, b ~~ Sequence) => a -> b -> Term Sequence
(++) a b = op UNION (a, b) []
concat = (++)

map, map' :: (a ~~ Sequence, f ~~~ Function '[Datum] Datum) => f -> a -> Term Sequence
map f a = op MAP (a, f) []
map' = map

filter', filter :: (a ~~ Sequence, f ~~~ Function '[Datum] Bool) => f -> a -> Term (SequenceType a)
filter f a = op FILTER (a, f) []
filter' = filter

between :: (a ~~ Datum, b ~~ Datum, s ~~ Sequence) => a -> b -> s -> Term (SequenceType s)
between a b e = op BETWEEN [e] ["left_bound" := a, "right_bound" := b]

append :: (a ~~ Datum, b ~~ Sequence) => a -> b -> Term Sequence
append a b = op APPEND (b, a) []

concatMap, concatMap' :: (f ~~~ Function '[Datum] Datum, a ~~ Sequence)
  => f -> a -> Term Sequence
concatMap f e = op CONCATMAP (e, f) []
concatMap' = concatMap

innerJoin, outerJoin :: (f ~~~ Function '[T.Object, T.Object] Bool, a ~~ Sequence, b ~~ Sequence)
          => f -> a -> b -> Term Stream
innerJoin f a b = op INNER_JOIN (a, b, f) []
outerJoin f a b = op OUTER_JOIN (a, b, f) []

eqJoin :: (a ~~ Sequence, b ~~ Sequence) => a -> Key -> b -> Term Stream
eqJoin a k b = op EQ_JOIN (a, expr k, b) []

drop, drop' :: (a ~~ Number, b ~~ Sequence) => a -> b -> Term (SequenceType b)
drop a b = op SKIP (b, a) []
drop' = drop

take, take' :: (a ~~ Number, b ~~ Sequence) => a -> b -> Term (SequenceType b)
take a b = op LIMIT (a, b) []
take' = take

slice :: (a ~~ Number, b ~~ Number, c ~~ Sequence) => a -> b -> c -> Term (SequenceType c)
slice n m s = op SLICE (s, n, m) []

(!!), nth :: (a ~~ Sequence, b ~~ Number) => a -> b -> Term (ElemType a)
nth n s = op NTH (s, n) []
s !! n = op NTH (s, n) []

fold :: (f ~~~ Function '[x, Datum] x, b ~~ x, s ~~ Sequence) => f -> b -> s -> Term x
fold f b s = op REDUCE (f, s) ["base" := b]

fold1 :: (f ~~~ Function '[x, Datum] x, s ~~ Sequence) => f -> s -> Term x
fold1 f s = op REDUCE (f, s) []

distinct :: (s ~~ Sequence) => s -> Term Sequence
distinct s = op DISTINCT [s] []

groupedMapReduce ::
  (group ~~~ Function '[Datum] Datum,
   map ~~~ Function '[T.Object] Datum,
   reduce ~~~ Function '[Datum, Datum] Datum)
   => group -> map -> reduce -> Term Stream
groupedMapReduce g m r = op GROUPED_MAP_REDUCE (g, m, r) []

forEach :: (s ~~ Sequence, f ~~~ Function '[Datum] Datum) => s -> f -> Term T.Object
forEach s f = op FOREACH (s, f) []

mergeRightLeft :: (a ~~ Sequence) => a -> Term Sequence
mergeRightLeft a = op ZIP [a] []

data Order = Asc  { orderAttr :: Key }
           | Desc { orderAttr :: Key }

orderBy :: (s ~~ Sequence) => [Order] -> s -> Term Sequence
orderBy o s = Term $ do
  s' <- baseTerm (expr s)
  o' <- baseArray $ arr $ P.map buildOrder o
  return $ BaseTerm ORDERBY P.Nothing (Cons s' o') []
  where
    buildOrder (Asc k) = op ASC [k] []
    buildOrder (Desc k) = op DESC [k] []

{-
groupBy,groupBy' :: (ToStream e, ObjectType `HasToStreamValueOf` e) =>
                    [String] -> MapReduce ObjectType b c d -> e -> Expr (StreamType False d)
groupBy ks (MapReduce m b r f) e = map f (groupedMapReduce (pick ks) m b r e)
groupBy' = groupBy
-}

sum, sum' :: (s ~~ Sequence) => s -> Term Number
sum = fold ((+) :: Term Number -> Term Number -> Term Number) (0 :: Term Number)
sum' = sum

avg :: (s ~~ Sequence) => s -> Term Number
avg s = sum s / count s

-- * Accessors

(!) :: (s ~~ Sequence) => s -> Key -> Term Datum
(!) s k = op GETATTR (s, k) []

pluck :: (o ~~ T.Object) => [Key] -> o -> Term T.Object
pluck ks e = op PLUCK (cons e $ arr (P.map expr ks)) []

without :: (o ~~ T.Object) => [Key] -> o -> Term T.Object
without ks e = op WITHOUT (cons e $ arr (P.map expr ks)) []

member :: (o ~~ T.Object) => [Key] -> o -> Term Bool
member ks o = op CONTAINS (cons o $ arr (P.map expr ks)) []

merge :: (a ~~ T.Object, b ~~ T.Object) => a -> b -> Term T.Object
merge a b = op MERGE (a, b) []

class Javascript r where
  js :: P.String -> r

instance Javascript (Term Datum) where
  js s = op JAVASCRIPT [str s] []

instance Javascript (Term Datum -> Term Datum) where
  js s x = op FUNCALL (op JAVASCRIPT [str s] [], x) []

instance Javascript (Term Datum -> Term Datum -> Term Datum) where
  js s x y = op FUNCALL (op JAVASCRIPT [str s] [], x, y) []

if' :: (a ~~ Bool, b ~~ Top, c ~~ Top) => a -> b -> c -> Term Top
if' a b c = op BRANCH (a, b, c) []

error, error' :: P.String -> Term Top
error m = op ERROR [str m] []
error' = error

-- | Create a Database reference
db :: P.String -> O.Database
db s = O.Database s
{-
-- | Create a database on the server
dbCreate :: String -> Query False Database
dbCreate db_name = Query
  (metaQuery $ return $ QL.MetaQuery QL.CREATE_DB (Just $ uFromString db_name) Nothing Nothing)
  (const $ Right $ Database db_name)

-- | Drop a database
dbDrop :: Database -> Query False ()
dbDrop (Database name) = Query
  (metaQuery $ return $ QL.MetaQuery QL.DROP_DB (Just $ uFromString name) Nothing Nothing)
  (const $ Right ())

-- | List the databases on the server
--
-- >>> run h $ dbList
-- [test, dev, prod]

dbList :: Query False [Database]
dbList = Query
  (metaQuery $ return $ QL.MetaQuery QL.LIST_DBS Nothing Nothing Nothing)
  (maybe (Left "error") Right . sequence . map (fmap Database . convert))

-- | Options used to create a table
data TableCreateOptions = TableCreateOptions {
  tableDataCenter :: Maybe String,
  tableCacheSize :: Maybe Int64
  }

instance Default TableCreateOptions where
  def = TableCreateOptions Nothing Nothing

-- | A table description
data Table = Table {
  tableDatabase :: Maybe Database, -- ^ when Nothing, use the rdbDatabase
  tableName :: String,
  _tablePrimaryAttr :: Maybe String -- ^ when Nothing, "id" is used
  } deriving (Eq, Ord)

instance Show Table where
  show (Table db' nam pa) =
    maybe "" (\(Database d) -> d++".") db' ++ nam ++ maybe "" (\x -> "{"++x++"}") pa

tablePrimaryAttr :: Table -> String
tablePrimaryAttr = fromMaybe (uToString defaultPrimaryAttr) . _tablePrimaryAttr

-- | "id"
defaultPrimaryAttr :: Utf8
defaultPrimaryAttr = uFromString "id"

-- | Create a simple table refence with no associated database or primary key
--
-- >>> table "music"
--
-- Another way to create table references is to use the Table constructor:
--
-- >>> Table (Just "mydatabase") "music" (Just "tuneid")

table :: String -> Table
table n = Table Nothing n Nothing

-- | Create a table on the server
--
-- @def@ can be imported from Data.Default
--
-- >>> t <- run h $ tableCreate (table "fruits") def

tableCreate :: Table -> TableCreateOptions -> Query False Table
tableCreate (Table mdb table_name primary_key)
  (TableCreateOptions datacenter cache_size) = Query
  (metaQuery $ do
      curdb <- activeDB
      let create = defaultValue {
        QLCreateTable.datacenter = fmap uFromString datacenter,
        QLCreateTable.table_ref = QL.TableRef (uFromString $ databaseName $ fromMaybe curdb mdb)
                                  (uFromString table_name) Nothing,
        QLCreateTable.primary_key = fmap uFromString primary_key,
        QLCreateTable.cache_size = cache_size
        }
      return $ QL.MetaQuery QL.CREATE_TABLE Nothing (Just create) Nothing)
               (const $ Right $ Table mdb table_name primary_key)

-- | Drop a table
tableDrop :: Table -> Query False ()
tableDrop tbl = Query
  (metaQuery $ do
      ref <- tableRef tbl
      return $ QL.MetaQuery QL.DROP_TABLE Nothing Nothing $ Just $ ref)
  (const $ Right ())

-- | List the tables in a database
tableList :: Database -> Query False [Table]
tableList (Database name) = Query
  (metaQuery $ return $
    QL.MetaQuery QL.LIST_TABLES (Just $ uFromString name) Nothing Nothing)
  (maybe (Left "error") Right . sequence .
   map (fmap (\x -> Table (Just (Database name)) x Nothing) . convert))

-- | Get the primary key of the table as a Utf8, or "id" if there is none
uTableKey :: Table -> Utf8
uTableKey (Table _ _ mkey) = fromMaybe defaultPrimaryAttr $ fmap uFromString mkey

-- | A reference to a document
data Document = Document {
  documentTable :: Table,
  documentKey :: Value
  } deriving (Eq)

instance Show Document where
  show (Document t k) = show t ++ "[" ++ show k ++ "]"

-- | Get a document by primary key
get :: (ToExpr e, ExprType e ~ StreamType True ObjectType, ToValue k) =>
       e -> k -> ObjectExpr
get e k = Expr $ do
  (vw, _) <- exprV e
  let tbl@(Table _ _ mattr) = viewTable vw
  ref <- tableRef tbl
  key <- value k
  withView NoView $ return defaultValue {
    QL.type' = QL.GETBYKEY,
    QL.get_by_key = Just $ QL.GetByKey ref (fromMaybe defaultPrimaryAttr $
                                            fmap uFromString mattr) key
    }

insert_or_upsert :: (ToValue a, ToValueType (ExprType a) ~ ObjectType) =>
                    Table -> [a] -> Bool -> WriteQuery [Document]
insert_or_upsert tbl array overwrite = WriteQuery
  (do ref <- tableRef tbl
      as <- mapM value array
      let write = defaultValue {
          QLWriteQuery.type' = QL.INSERT,
          QL.insert = Just $ QL.Insert ref
                      (Seq.fromList $ as) (Just overwrite) }
      return $ write)
  (whenSuccess "generated_keys" $ \keys -> Right $ map (\doc -> Document tbl doc) keys)

-- | Insert a document into a table
--
-- >>> d <- run h $ insert t (object ["name" .= "banana", "color" .= "red"])

insert :: (ToValue a, ToValueType (ExprType a) ~ ObjectType) =>
          Table -> a -> WriteQuery Document
insert tb a = fmap head $ insert_or_upsert tb [a] False

-- | Insert many documents into a table
insertMany :: (ToValue a, ToValueType (ExprType a) ~ ObjectType) =>
              Table -> [a] -> WriteQuery [Document]
insertMany tb a = insert_or_upsert tb a False

-- | Insert a document into a table, overwriting a document with the
--   same primary key if one exists.

upsert :: (ToValue a, ToValueType (ExprType a) ~ ObjectType) =>
          Table -> a -> WriteQuery Document
upsert tb a = fmap head $ insert_or_upsert tb [a] True

-- | Insert many documents into a table, overwriting any existing documents
--   with the same primary key.
upsertMany :: (ToValue a, ToValueType (ExprType a) ~ ObjectType) =>
              Table -> [a] -> WriteQuery [Document]
upsertMany tb a = insert_or_upsert tb a True

-- | Update a table
--
-- >>> t <- run h $ tableCreate (table "example") def
-- >>> run h $ insertMany t [object ["a" .= 1, "b" .= 11], object ["a" .= 2, "b" .= 12]]
-- >>> run h $ update t (object ["b" .= 20])
-- >>> run h $ t

update :: (ToExpr sel, ExprType sel ~ StreamType True out, ToMapping map,
           MappingFrom map ~ out, MappingTo map ~ ObjectType) =>
          sel -> map -> WriteQuery ()
update view m = WriteQuery
  (do mT <- mapping m
      write <- case toExpr view of
        Expr _ -> do viewT <- expr view
                     return defaultValue {
                       QLWriteQuery.type' = QL.UPDATE,
                       QL.update = Just $ QL.Update viewT mT }
        SpotExpr (Document tbl@(Table _ _ k) d) -> do
          ref <- tableRef tbl
          return $ defaultValue {
            QLWriteQuery.type' = QL.POINTUPDATE,
            QL.point_update = Just $ QL.PointUpdate ref
                              (fromMaybe defaultPrimaryAttr $ fmap uFromString k)
                              (toJsonTerm d) mT }
      return write)
  (whenSuccess_ ())

-- | Replace documents in a table
replace :: (ToExpr sel, ExprIsView sel ~ True, ToJSON a) => sel -> a -> WriteQuery ()
replace view a = WriteQuery
  (do fun <- mapping (toJSON a)
      write <- case toExpr view of
        Expr f -> do
          (_, e) <- f
          return defaultValue {
            QLWriteQuery.type' = QL.MUTATE,
            QL.mutate = Just $ QL.Mutate e fun }
        SpotExpr (Document tbl@(Table _ _ k) d) -> do
          ref <- tableRef tbl
          return defaultValue {
            QLWriteQuery.type' = QL.POINTMUTATE,
            QL.point_mutate = Just $ QL.PointMutate ref
                              (fromMaybe defaultPrimaryAttr $ fmap uFromString k)
                              (toJsonTerm d) fun }
      return write)
  (whenSuccess_ ())

-- | Delete one or more documents from a table
delete :: (ToExpr sel, ExprIsView sel ~ True) => sel -> WriteQuery ()
delete view = WriteQuery
  (do write <- case toExpr view of
          Expr f -> do
            (_, ex) <- f
            return defaultValue {
              QLWriteQuery.type' = QL.DELETE,
              QL.delete = Just $ QL.Delete ex }
          SpotExpr (Document tbl@(Table _ _ k) d) -> do
            ref <- tableRef tbl
            return defaultValue {
              QLWriteQuery.type' = QL.POINTDELETE,
              QL.point_delete = Just $ QL.PointDelete ref
                              (fromMaybe defaultPrimaryAttr $ fmap uFromString k)
                              (toJsonTerm d) }
      return write)
  (whenSuccess_ ())
-}
