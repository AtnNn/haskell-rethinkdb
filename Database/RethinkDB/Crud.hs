module Database.RethinkDB.Crud where

-- | A database, referenced by name
data Database = Database {
  databaseName :: String
  } deriving (Eq, Ord)

instance Show Database where
  show (Database d) = show d

-- | Create a Database reference
db :: String -> Database
db s = Database s

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
