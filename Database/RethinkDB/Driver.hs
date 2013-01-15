{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, MultiParamTypeClasses, 
             TypeFamilies, ExistentialQuantification, FlexibleInstances, 
             ConstraintKinds, FlexibleContexts, UndecidableInstances, 
             PolyKinds, FunctionalDependencies, GADTs, ScopedTypeVariables, 
             RankNTypes, RecordWildCards #-}

-- | The core of the haskell client library for RethinkDB

module Database.RethinkDB.Driver where

import Debug.Trace (trace, putTraceMsg)

import {-# SOURCE #-} Database.RethinkDB.Functions
import Database.RethinkDB.Types

import Control.Arrow
import Data.Aeson.Types (parseMaybe)
import Data.String
import Data.Typeable
import Data.Data
import GHC.Prim

import qualified Database.RethinkDB.Internal.Types as QL
import qualified Database.RethinkDB.Internal.Query_Language.Response as QLResponse
import qualified Database.RethinkDB.Internal.Query_Language.Query as QLQuery
import qualified Database.RethinkDB.Internal.Query_Language.VarTermTuple as QLVarTermTuple
import qualified Database.RethinkDB.Internal.Query_Language.Predicate as QLPredicate
import qualified Database.RethinkDB.Internal.Query_Language.Builtin as QLBuiltin
import qualified Database.RethinkDB.Internal.Query_Language.ReadQuery as QLReadQuery
import qualified Database.RethinkDB.Internal.Query_Language.WriteQuery as QLWriteQuery
import qualified Database.RethinkDB.Internal.Query_Language.Mapping as QLMapping
import qualified Database.RethinkDB.Internal.Query_Language.Term as QL (type')
import qualified Database.RethinkDB.Internal.Query_Language.MetaQuery.CreateTable as QLCreateTable

import Text.ProtocolBuffers.Basic hiding (Default)
import Text.ProtocolBuffers.WireMessage

import System.IO (Handle, hClose)
import Network

import Data.List
import Control.Monad.State as S
import qualified Data.HashMap.Strict as HM
import Data.Default
import qualified Data.Attoparsec.Lazy as Attoparsec
import Data.Foldable (toList)
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Aeson.Parser (value)
import qualified Data.Sequence as Seq
import Data.Monoid
import Data.Maybe
import Data.ByteString.Lazy (pack, unpack, hPut, hGet)
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Data.Bits

-- * Network

-- | A connection to the database server
data RethinkDBHandle = RethinkDBHandle {
  rdbHandle :: Handle, 
  rdbToken :: IORef Int64, -- ^ The next token to use
  rdbDatabase :: Database  -- ^ When no database is specified, this one will be used
  }

-- | Create a new connection to the database server
-- 
-- /Example:/ connect using the default port (28015) and specifying the
-- default database for all queries.
-- 
-- >>> h <- openConnection "localhost" Nothing (Just "test")

openConnection :: HostName -> Maybe PortID -> Maybe String -> IO RethinkDBHandle
openConnection host port mdb = do
  h <- connectTo host (fromMaybe (PortNumber 28015) port)
  hPut h initialMessage
  r <- newIORef 1
  -- db' <- maybe (fmap head $ run (RethinkDBHandle h r (Database "")) $ dbList) (return . Database) mdb
  let db' = Database $ fromMaybe "" mdb
  return (RethinkDBHandle h r db')
  where initialMessage = packUInt 0xaf61ba35

-- | Set the default connection
-- 
-- The new handle is an alias for the old one. Calling closeConnection on either one
-- will close both.
-- 
-- >>> let h' = h `use` (db "players")

use :: RethinkDBHandle -> Database -> RethinkDBHandle
use h db' = h { rdbDatabase = db' }

-- | Close an open connection
-- 
-- >>> closeConnection h

closeConnection :: RethinkDBHandle -> IO ()
closeConnection (RethinkDBHandle h _ _) = hClose h

-- | Receive a fixed amoutn of data
recvAll :: RethinkDBHandle -> Int -> IO ByteString
recvAll (RethinkDBHandle h _ _) n = hGet h n

-- | Send a bytestring
sendAll :: RethinkDBHandle -> ByteString -> IO ()
sendAll (RethinkDBHandle h _ _) s = hPut h s

-- | Get a request token and increment the token counter
getNewToken :: RethinkDBHandle -> IO Int64
getNewToken (RethinkDBHandle _ r _) = atomicModifyIORef r $ \t -> (t + 1, t)

data ErrorCode = ErrorBrokenClient
               | ErrorBadQuery
               | ErrorRuntime
               | ErrorNetwork

instance Show ErrorCode where
  show ErrorBrokenClient = "broken client error"
  show ErrorBadQuery = "malformed query error"
  show ErrorRuntime = "runtime error"
  show ErrorNetwork = "error talking to server"

data SuccessCode = SuccessEmpty
                 | SuccessJson
                 | SuccessPartial
                 | SuccessStream
                 deriving Show

-- | The raw response to a query
data Response = ErrorResponse {
  errorCode :: ErrorCode,
  errorMessage :: String,
  errorBacktrace :: [String]
  } | SuccessResponse {
  successCode :: SuccessCode,
  successString :: [B.ByteString]
  }

instance Show Response where
  show ErrorResponse {..} = show errorCode ++ ": " ++
                            show errorMessage ++ " (" ++
                            showBacktrace errorBacktrace ++ ")"
  show SuccessResponse {..} = show successCode ++ ": " ++ show successString

convertResponse :: Either String QL.Response -> Response
convertResponse (Left s) = ErrorResponse ErrorNetwork s []
convertResponse (Right QL.Response {..}) = case status_code of
  QL.SUCCESS_EMPTY   -> SuccessResponse SuccessEmpty   r
  QL.SUCCESS_JSON    -> SuccessResponse SuccessJson    r
  QL.SUCCESS_PARTIAL -> SuccessResponse SuccessPartial r
  QL.SUCCESS_STREAM  -> SuccessResponse SuccessStream  r
  QL.BROKEN_CLIENT   -> ErrorResponse ErrorBrokenClient e bt
  QL.BAD_QUERY       -> ErrorResponse ErrorBadQuery     e bt
  QL.RUNTIME_ERROR   -> ErrorResponse ErrorRuntime      e bt
  where bt = fromMaybe [] $ fmap (map uToString . toList . QL.frame) backtrace
        r = map utf8 $ toList response
        e = fromMaybe "error" $ fmap uToString error_message

showBacktrace :: [String] -> String
showBacktrace [] = "query"
showBacktrace bt = ("in " ++ ) . concat . (++ [" query"]) .
                   intersperse " " . map f . reverse $ bt
  where f x = x ++ " in"

-- | Execute a raw protobuffer query and return the raw response
runQLQuery :: RethinkDBHandle -> QL.Query -> IO Response
runQLQuery h query = do
  let queryS = messagePut query
  putTraceMsg $ "haskell-rethinkdb: Sending Query (token #" ++ show (QLQuery.token query) ++ ")"
  sendAll h $ packUInt (fromIntegral $ B.length queryS) <> queryS
  fmap convertResponse $ readResponse (QLQuery.token query)
  where readResponse t = do
          putTraceMsg $ "haskell-rethinkdb: Waiting for the next message from the server"
          header <- recvAll h 4
          putTraceMsg $ "haskell-rethinkdb: Preparing to receive " ++ show (unpackUInt header) ++ " bytes"
          responseS <- recvAll h (unpackUInt header)
          let eResponse = messageGet responseS
          case eResponse of
            Left errMsg -> return $ Left errMsg
            Right (response, rest)
              | B.null rest -> do
                putTraceMsg $ "haskell-rethinkdb: Got a message with token #" ++
                  show (QLResponse.token response)
                (case QLResponse.token response of
                  n | n == t -> return $ Right response
                    | n > t -> return $ Left "RethinkDB: runQLQuery: invalid response token"
                    | otherwise -> do
                      putTraceMsg $ "haskell-rethinkdb: Ignoring the message"
                      readResponse t)
              | otherwise -> return $ Left "RethinkDB: runQLQuery: invalid reply length"

-- * CRUD

-- | A database, referenced by name
data Database = Database {
  databaseName :: String
  } deriving (Eq, Ord)

instance Show Database where
  show (Database d) = show d

-- | Create a Database reference
db :: String -> Database
db s = Database s

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

-- * Queries

-- | A query returning a
data Query (b :: Bool) a where
  Query :: { _queryBuild :: QueryM QL.Query, 
             _queryExtract :: [Value] -> Either String a } -> Query False a
  ViewQuery ::  { _viewQueryBuild :: QueryM (Table, QL.Query),
                  _viewQueryExtract :: [(Document, Value)] -> Either String a }
                -> Query True a

data WriteQuery a = WriteQuery {
  writeQueryBuild :: QueryM QL.WriteQuery,
  writeQueryExtract :: [Value] -> Either String a
  }

queryBuild :: Query w a -> QueryM (MaybeView w, QL.Query)
queryBuild (Query b _) = fmap ((,) NoView) b
queryBuild (ViewQuery b _) = fmap (first ViewOf) b

queryExtract :: Query w a -> MaybeView w -> [Value] -> Either String a
queryExtract (Query _ e)     _            vs = e vs
queryExtract (ViewQuery _ e) (ViewOf tbl) vs = e =<< mapM (addDoc tbl) vs
queryExtract _ _ _ = error "GHC was right, this branch is reachable!"

instance Functor (Query w) where
  fmap f (Query a g) = Query a (fmap f . g)
  fmap f (ViewQuery a g) = ViewQuery a (fmap f . g)

instance Functor WriteQuery where
  fmap f (WriteQuery a g) = WriteQuery a (fmap f . g)


type family If (p :: Bool) (a :: k) (b :: k) :: k
type instance If True  a b = a
type instance If False a b = b


-- | Convert things like tables and documents into queries
class ToBuildQuery a where
  type BuildViewQuery a :: Bool
  buildQuery :: a -> ([If (BuildViewQuery a)
                       (Document, Value) Value]
                      -> Either String b) -> Query (BuildViewQuery a) b

class ToBuildQuery a => ToQuery a b | a -> b where
  toQuery :: a -> Query (BuildViewQuery a) b

instance ToBuildQuery (Query w a) where
  type BuildViewQuery (Query w a) = w
  buildQuery (Query b _) = Query b
  buildQuery (ViewQuery b _) = ViewQuery b

instance ToQuery (Query w a) a where
  toQuery q = q

instance ToBuildQuery (WriteQuery a) where
  type BuildViewQuery (WriteQuery a) = False
  buildQuery (WriteQuery b _) = Query $ do
    wq <- b
    tok <- getToken
    return $ defaultValue { QLQuery.type' = QL.WRITE, QLQuery.token = tok, 
                            QLQuery.write_query = Just $ wq }  

instance ToQuery (WriteQuery a) a where
  toQuery w@(WriteQuery _ e) = buildQuery w e

instance ToBuildQuery Table where
  type BuildViewQuery Table = True
  buildQuery = buildQuery . toExpr

instance FromJSON a => ToQuery Table [(Document, a)] where
  toQuery tbl = buildQuery tbl $
    (maybe (Left "wrong response type") Right . mapMSnd convert)

mapMSnd :: Monad m => (a -> m b) -> [(c,a)] -> m [(c,b)]
mapMSnd f = mapM $ \(a,b) -> liftM ((,) a) (f b)

instance ToBuildQuery Document where
  type BuildViewQuery Document = True
  buildQuery = buildQuery . toExpr

instance FromJSON a => ToQuery Document a where
  toQuery doc = buildQuery doc $
    maybe (Left "empty response") Right . ((convert . snd) <=< listToMaybe)

data Proxy t = Proxy

instance ToBuildQuery (Expr (StreamType False v)) where
  type BuildViewQuery (Expr (StreamType False v)) = False
  buildQuery = Query . fmap snd . exprToQLQuery

instance ExtractValue v a => ToQuery (Expr (StreamType False v)) [a] where
  toQuery e = buildQuery e $
              maybe (Left "cannot convert response") Right
              . extractListOf (Proxy :: Proxy v)

instance ToBuildQuery (Expr (StreamType True v)) where
  type BuildViewQuery (Expr (StreamType True v)) = True
  buildQuery = ViewQuery . fmap (first viewTable) . exprToQLQuery

instance ExtractValue v a => ToQuery (Expr (StreamType True v)) [(Document, a)] where
  toQuery e = exprViewQuery (maybe (Left "cannot convert response") Right . extract) e
    where extract = extractListOf (Proxy :: Proxy v)

instance ToBuildQuery (Expr (ValueType v)) where
  type BuildViewQuery (Expr (ValueType v)) = False
  buildQuery = Query . fmap snd . exprToQLQuery

instance ExtractValue v a => ToQuery (Expr (ValueType v)) a where
  toQuery e = buildQuery e $ 
              maybe (Left "empty response")
                (maybe (Left "cannot convert response") Right .
                 extractValue (Proxy :: Proxy v))
              . listToMaybe

exprToQLQuery :: Expr t -> QueryM (MaybeView (ExprTypeIsView t), QL.Query)
exprToQLQuery e = do
  token <- getToken
  (vw, ex) <- exprV e
  return $ (,) vw $ QL.Query QL.READ token (
    Just $ defaultValue { QLReadQuery.term = ex }) Nothing Nothing

exprViewQuery :: (ExprTypeIsView t ~ True) =>
                 ([Value] -> Either String [a]) -> Expr t -> Query True [(Document, a)]
exprViewQuery c e = flip ViewQuery (\x -> fmap (zip $ map fst x) (c (map snd x))) $ do
  token <- getToken
  (ViewOf tbl, ex) <- exprV e
  return $ (,) tbl $ QL.Query QL.READ token (
    Just $ defaultValue { QLReadQuery.term = ex }) Nothing Nothing

extractListOf :: ExtractValue t a => Proxy t -> [Value] -> Maybe [a]
extractListOf p = sequence . map (extractValue p)

class ExtractValue t v | t -> v where
  extractValue :: Proxy t -> Value -> Maybe v

instance (Num a, FromJSON a) => ExtractValue NumberType a where extractValue _ = convert
instance ExtractValue BoolType Bool where extractValue _ = convert
instance FromJSON a => ExtractValue ObjectType a where extractValue _ = convert
instance FromJSON a => ExtractValue ArrayType a where extractValue _ = convert
instance (IsString a, FromJSON a) => ExtractValue StringType a where extractValue _ = convert
instance ExtractValue NoneType () where extractValue _ = const $ Just ()
instance FromJSON a => ExtractValue OtherValueType a where extractValue _ = convert

-- | Run a query on the connection
-- 
-- The return value depends on the type of the second argument.
-- 
-- When the return value is polymorphic, type annotations may be required.
-- 
-- >>> run h $ table "fruits" :: IO [(Document, Value)]

run :: ToQuery a v => RethinkDBHandle -> a -> IO v
run h q = do
  r <- runEither h q
  case r of
    Left e -> error e
    Right a -> return a

-- | Run a query on the connection, returning (Left message) on error
runEither :: ToQuery a v => RethinkDBHandle -> a -> IO (Either String v)
runEither h q = case toQuery q of
  -- TODO: just call runBatch and get the resultsSeq
  query -> do
    tok <- getNewToken h
    let ((vw, qlq), _) = runQueryM (queryBuild query) $
                    QuerySettings tok (rdbDatabase h) initialVars False
    r <- runQLQuery h qlq
    return $ case r of
      ErrorResponse {} -> Left (show r)
      SuccessResponse _ strs -> queryExtract query vw =<<
       (maybe (Left "decode error") Right . sequence . map decodeAny $ strs)

addDoc :: Table -> Value -> Either String (Document, Value)
addDoc tbl x = do
  id' <- maybe (Left "missign primary key") Right $ getKey (tablePrimaryAttr tbl) x
  return (Document tbl id', x)
  where getKey k (Object o) = parseMaybe (.: str k) o
        getKey _ _ = Nothing

-- | Run a query on the connection, returning Nothing on error
runMaybe :: ToQuery a v => RethinkDBHandle -> a -> IO (Maybe v)
runMaybe h = fmap (either (const Nothing) Just) . runEither h

-- | Run a query on the connection and return the raw response
runRaw :: (ToBuildQuery q, JSONQuery (BuildViewQuery q)) =>
          RethinkDBHandle -> q -> IO Response
runRaw h q = do
  tok <- getNewToken h
  let ((_, qlq), _) = runQueryM (queryBuild (jsonQuery (buildQuery q))) $
                      QuerySettings tok (rdbDatabase h) initialVars False
  runQLQuery h qlq

-- | Run a query on the connection and return the resulting JSON value
runJSON :: (JSONQuery (BuildViewQuery q), ToBuildQuery q) =>
           RethinkDBHandle -> q -> IO [Value]
runJSON h q = run h (jsonQuery (buildQuery q))

class JSONQuery (b :: Bool) where
  jsonQuery :: (forall a . ([If b (Document, Value) Value] -> Either String a) -> Query b a)
               -> Query b [Value]

instance JSONQuery False where
  jsonQuery f = f Right

instance JSONQuery True where
  jsonQuery f = f (Right . map snd)

data Results a = Results {
  resultsHandle :: IORef (Maybe (RethinkDBHandle, Int64)),
  resultsSeq :: IORef (Seq.Seq a),
  _resultsError :: IORef (Maybe String),
  resultsQueryView :: QueryViewPair [a]
  }

data QueryViewPair a where
  QueryViewPair :: Query w a -> MaybeView w -> QueryViewPair a

-- | Run a query on the connection and a return a lazy result list
-- 
-- >>> res <- runBatch h <- (arrayToStream [1,2,3] :: NumberStream)
-- >>> next res
-- Just 1
-- >>> collect res
-- [2,3]

runBatch :: ToQuery q [a] => RethinkDBHandle -> q -> IO (Results a)
runBatch h q = case toQuery q of
  query -> do
    tok <- getNewToken h
    let ((vw, qlq), _) = runQueryM (queryBuild query) $
                    QuerySettings tok (rdbDatabase h) initialVars False
    r <- runQLQuery h qlq
    let (han, seq', err) = queryExtractResponse query vw r h tok
    refHan <- newIORef han
    refSeq <- newIORef seq'
    refErr <- newIORef err
    return $ Results refHan refSeq refErr (QueryViewPair query vw)

queryExtractResponse ::
  Query w [a] -> MaybeView w -> Response -> RethinkDBHandle -> Int64
  -> (Maybe (RethinkDBHandle, Int64), Seq a, Maybe String)
queryExtractResponse query vw r h tok =
    case r of
      ErrorResponse {} -> (Nothing, Seq.fromList [], Just $ show r)
      SuccessResponse typ strs ->
        let rList = queryExtract query vw =<<
               (maybe (Left "decode error") Right . sequence . map decodeAny $ strs)
        in case rList of
          Left err -> (Nothing, Seq.fromList [], Just err)
          Right list ->
            let han = case typ of
                  SuccessPartial -> Just (h, tok)
                  SuccessStream -> Nothing
                  SuccessJson -> Nothing
                  SuccessEmpty -> Nothing
            in (han, Seq.fromList list, Nothing)

-- | Read the next value from a lazy query. Fetch it from the server if needed.
next :: Results a -> IO (Maybe a)
next res = do
  seq' <- readIORef (resultsSeq res)
  case Seq.viewl seq' of
    car Seq.:< cdr -> do
      writeIORef (resultsSeq res) cdr
      return (Just car)
    Seq.EmptyL -> do
      mh <- readIORef (resultsHandle res)
      case (mh, resultsQueryView res) of
        (Nothing, _) -> return Nothing
        (Just (h, tok), (QueryViewPair query vw)) -> do
          resp <- runQLQuery h $ defaultValue {
            QLQuery.type' = QL.CONTINUE, QLQuery.token = tok }
          let (han, seq'', err) = queryExtractResponse query vw resp h tok
          writeIORef (resultsHandle res) han
          modifyIORef (resultsSeq res) (<> seq'')
          writeIORef (_resultsError res) err
          next res

-- | Return all the results of a lazy query.
collect :: Results a -> IO [a]
collect r = do
  ma <- next r
  case ma of
    Nothing -> return []
    Just a -> fmap (a:) (collect r)

-- | Get the last error from a lazy query.
-- 
-- If both next and resultsError return Nothing, then
-- all results have been fetched without error.

resultsError :: Results a -> IO (Maybe String)
resultsError = readIORef . _resultsError

-- * Expressions

-- | Can the Expr be written to? (updated or deleted)
type family ExprTypeIsView (expr :: ExprTypeKind) :: Bool
type instance ExprTypeIsView (StreamType w o) = w
type instance ExprTypeIsView (ValueType v) = False

type ExprIsView e = ExprTypeIsView (ExprType e)

type family ExprTypeNoView (t :: ExprTypeKind) :: ExprTypeKind
type instance ExprTypeNoView (StreamType b t) = StreamType False t
type instance ExprTypeNoView (ValueType t) = ValueType t

-- | The type of the value of an Expr
type family ExprValueType expr :: ValueTypeKind
type instance ExprValueType (Expr (ValueType v)) = v
type instance ExprValueType (Expr (StreamType w v)) = v

-- | The type of the stream of an Expr
type family ExprTypeStreamType (t :: ExprTypeKind) :: ValueTypeKind
type instance ExprTypeStreamType (StreamType w t) = t

-- | An RQL expression
data Expr (t :: ExprTypeKind) where
  Expr :: QueryM (MaybeView (ExprIsView (Expr t)), QL.Term) -> Expr t
  SpotExpr :: Document -> Expr (StreamType True ObjectType)

mkExpr :: ExprIsView (Expr t) ~ False => QueryM QL.Term -> Expr t
mkExpr q = Expr $ fmap ((,) NoView) q

mkView :: ExprIsView (Expr t) ~ True => Table -> QueryM QL.Term -> Expr t
mkView t q = Expr $ fmap ((,) (ViewOf t)) q

viewKeyAttr :: MaybeView b -> Utf8
viewKeyAttr v = fromMaybe defaultPrimaryAttr $ fmap uFromString $
    case v of 
      ViewOf (Table _ _ k) -> k
      NoView -> Nothing

viewTable :: MaybeView True -> Table
viewTable v = case v of ViewOf t -> t

data MaybeView (w :: Bool) where
 NoView :: MaybeView False
 ViewOf :: Table -> MaybeView True

-- | Convert something into an Expr
class ToExpr (o :: *) where
  type ExprType o :: ExprTypeKind
  toExpr :: o -> Expr (ExprType o)

-- | The result type of toValue
type family ToValueType (t :: ExprTypeKind) :: ValueTypeKind
type instance ToValueType (StreamType w t) = ArrayType
type instance ToValueType (ValueType t) = t

-- | Convert something into a value Expr
class ToValue e where
  toValue :: e -> Expr (ValueType (ToValueType (ExprType e)))

type family FromMaybe (a :: k) (m :: Maybe k) :: k
type instance FromMaybe a Nothing = a
type instance FromMaybe a (Just b) = b

type HasToStreamValueOf a b = FromMaybe a (ToStreamValue b) ~ a

type ToStreamValue e = ToStreamTypeValue (ExprType e)

type family ToStreamTypeValue (t :: ExprTypeKind) :: Maybe ValueTypeKind
type instance ToStreamTypeValue (StreamType w t) = Just t
type instance ToStreamTypeValue (ValueType t) = Nothing

class ToExpr e => ToStream e where
  toStream :: e -> Expr (StreamType (ExprIsView e) (FromMaybe a (ToStreamValue e)))

instance ToExpr Document where
  type ExprType Document = StreamType True 'ObjectType
  toExpr doc = SpotExpr doc

instance ToValue Document where
  toValue = streamToArray

instance ToStream Document where
  toStream = toExpr

instance ToExpr Table where
  type ExprType Table = StreamType True ObjectType
  toExpr tbl = mkView tbl $ do
    ref <- tableRef tbl
    return defaultValue { 
      QL.type' = QL.TABLE,
      QL.table = Just $ QL.Table ref }

instance ToValue Table where
  toValue = streamToArray
                                  
instance ToStream Table where
  toStream = toExpr

instance ToExpr (Expr t) where
  type ExprType (Expr t) = t
  toExpr e = e

instance ToValue (Expr (ValueType t)) where
  toValue e = e

instance ToValue (Expr (StreamType w t)) where
  toValue = streamToArray

instance ToStream (Expr (ValueType ArrayType)) where
  toStream = arrayToStream

instance ToStream (Expr (StreamType w t)) where
  toStream e = e

instance ToExpr Int where
  type ExprType Int = ValueType NumberType
  toExpr n = mkExpr $ return $ defaultValue {
    QL.type' = QL.NUMBER, QL.number = Just $ fromIntegral n }

instance ToValue Int where
  toValue = toExpr
  
instance ToExpr Integer where
  type ExprType Integer = ValueType NumberType
  toExpr n = mkExpr $ return $ defaultValue {
    QL.type' = QL.NUMBER, QL.number = Just $ fromInteger n }

instance ToValue Integer where
  toValue = toExpr

instance ToExpr Double where
  type ExprType Double = ValueType NumberType
  toExpr n = mkExpr $ return $ defaultValue {
    QL.type' = QL.NUMBER, QL.number = Just n }

instance ToValue Double where
  toValue = toExpr

instance ToExpr Char where
  type ExprType Char = ValueType StringType
  toExpr c = mkExpr $ return defaultValue {
    QL.type' = QL.STRING, QL.valuestring = Just $ uFromString [c] }

instance ToExpr T.Text where
  type ExprType T.Text = ValueType StringType
  toExpr s = mkExpr $ return defaultValue {
    QL.type' = QL.STRING, QL.valuestring = Just $ uFromString (T.unpack s) }

instance ToValue T.Text where
  toValue = toExpr

str :: String -> T.Text
str = T.pack

instance ToExpr a => ToExpr [a] where
  type ExprType [a] = ValueType ArrayType
  toExpr l = mkExpr $ do
    exs <- sequence $ map (expr . toExpr) l
    return defaultValue {
      QL.type' = QL.ARRAY, QL.array = Seq.fromList exs }

instance ToExpr a => ToValue [a] where
  toValue = toExpr

instance ToExpr a => ToStream [a] where
  toStream = arrayToStream . toExpr

instance ToExpr () where
  type ExprType () = ValueType NoneType
  toExpr () = mkExpr $ return $  defaultValue { QL.type' = QL.JSON_NULL }

instance ToValue () where
  toValue = toExpr

instance ToExpr Obj where
  type ExprType Obj = ValueType ObjectType
  toExpr (Obj o) = mkExpr $ do
    exs <- sequence $ map go o
    return defaultValue {
      QL.type' = QL.OBJECT, QL.object = Seq.fromList exs }
    where go (k := a) = do
            ex <- value a
            return QL.VarTermTuple {
              QLVarTermTuple.var = uFromString k, QLVarTermTuple.term = ex }

instance ToValue Obj where
  toValue = toExpr

-- | Aliases for type constraints on expressions
type HasValueType a v = (ToValue a, ToValueType (ExprType a) ~ v)
type HaveValueType a b v = (HasValueType a v, HasValueType b v)

-- | Simple aliases for different Expr types
type NumberExpr = Expr (ValueType NumberType)
type BoolExpr = Expr (ValueType BoolType)
type ObjectExpr = Expr (ValueType ObjectType)
type ArrayExpr = Expr (ValueType ArrayType)
type StringExpr = Expr (ValueType StringType)
type ValueExpr t = Expr (ValueType t)
type NumberStream = Expr (StreamType False NumberType)
type BoolStream = Expr (StreamType False BoolType)
type ArrayStream = Expr (StreamType False ArrayType)
type StringStream = Expr (StreamType False StringType)
type ObjectStream = Expr (StreamType False ObjectType)
type Selection = Expr (StreamType True ObjectType)

-- | What values can be compared with eq, ne, lt, gt, le and ge
class CanCompare (a :: ValueTypeKind)
instance CanCompare NumberType
instance CanCompare StringType

instance Num (Expr (ValueType NumberType)) where
  (+) = add
  (-) = sub
  (*) = mul
  abs = jsfun "abs"
  signum = signum'
  fromInteger = toExpr

instance Fractional (Expr (ValueType NumberType)) where
  fromRational n = toExpr (fromRational n :: Double)
  (/) = div'

-- | A sequence is either a stream or an array
class Sequence (e :: ExprTypeKind) where
  type SequenceType e (t :: ValueTypeKind) :: Constraint

instance Sequence (StreamType w t) where
  type SequenceType (StreamType w t) tt = t ~ tt

instance a ~ ArrayType => Sequence (ValueType a) where
  type SequenceType (ValueType a) t = ()

-- | A list of String/Expr pairs
data Obj = Obj [Attribute]
data Attribute = forall e . (ToValue e) => String := e

-- | Build an Obj
obj :: [Attribute] -> Obj
obj = Obj

-- | Convert a stream into an array

streamToArray :: (ToExpr e, ExprType e ~ StreamType w t) => e -> Expr (ValueType ArrayType)
streamToArray = simpleOp QL.STREAMTOARRAY . return . expr

-- | Convert an array into a stream
arrayToStream :: (ToExpr e, ExprType e ~ ValueType ArrayType) => e -> Expr (StreamType False t)
arrayToStream = simpleOp QL.ARRAYTOSTREAM . return . expr

-- * Mappings

-- | A mapping is a like single-parameter function
data Mapping (from :: ValueTypeKind) (to :: ValueTypeKind) =
  Mapping (QueryM QL.Mapping)

-- | Convert objects into mappings
class ToMapping map where
  type MappingFrom map :: ValueTypeKind
  type MappingTo map :: ValueTypeKind
  toMapping :: map -> Mapping (MappingFrom map) (MappingTo map)

instance ToMapping Obj where
  type MappingFrom Obj = ObjectType
  type MappingTo Obj = ObjectType
  toMapping v = Mapping $ do 
    ex <- expr v
    return $ defaultValue { QLMapping.body = ex }

instance ToMapping Value where
  type MappingFrom Value = ObjectType
  type MappingTo Value = ObjectType
  toMapping v = Mapping $ return $ defaultValue { QLMapping.body = toJsonTerm v }

instance (ToValue b, a ~ Expr (ValueType t)) => ToMapping (a -> b) where 
  type MappingFrom (a -> b) = ExprValueType a
  type MappingTo (a -> b) = ToValueType (ExprType b)
  toMapping f = Mapping $ do
    v <- newVar
    ex <- value (f (var v))
    return $ defaultValue {
      QLMapping.arg = uFromString v,
      QLMapping.body = ex }

instance ToMapping (Expr (ValueType t)) where
  type MappingFrom (Expr (ValueType t)) = ObjectType
  type MappingTo (Expr (ValueType t)) = t
  toMapping e = Mapping $ do 
    ex <- expr e
    return defaultValue { QLMapping.body = ex }

-- * QueryM Monad

data QuerySettings = QuerySettings {
  _queryToken :: Int64,
  _queryDB :: Database,
  _queryVars :: [String],
  _queryUseOutdated :: Bool
  }

instance Default QuerySettings where
  def = QuerySettings 0 (db "") initialVars False

type QueryM = State QuerySettings

runQueryM :: QueryM a -> QuerySettings -> (a, QuerySettings)
runQueryM = runState

initialVars :: [String]
initialVars = concat $ zipWith (\n as -> map (++ (if n == 0 then "" else show n)) as)
              [0 :: Int ..] (map (map return) $ repeat ['a'..'z'])

getToken :: QueryM Int64
getToken = fmap _queryToken S.get

activeDB :: QueryM Database
activeDB = fmap _queryDB S.get

newVar :: QueryM String
newVar  = state $ \s -> let (x:xs) = _queryVars s in (x, s { _queryVars = xs} )

setUseOutdated :: ToExpr e => Bool -> e -> Expr (ExprType e)
setUseOutdated b e = Expr $ do
  state $ \s -> runQueryM (exprV e) s { _queryUseOutdated = b }

-- * Utilities

-- | Convert a protobuf Mapping into a Predicate
mappingToPredicate :: QL.Mapping -> QL.Predicate
mappingToPredicate (QL.Mapping arg body _1) = defaultValue {
  QLPredicate.arg = arg,
  QLPredicate.body = body
  }

-- | Convert a table to a raw protobuf term
tableToTerm :: Table -> QueryM QL.Term
tableToTerm tbl = do
  ref <- tableRef tbl
  return $ defaultValue {
    QL.type' = QL.TABLE,
    QL.table = Just $ QL.Table ref }

-- | Convert into a raw protobuf mapping
mapping :: ToMapping m => m -> QueryM QL.Mapping
mapping m = case toMapping m of Mapping f -> f

-- | Convert an Expr to a term
expr ::  ToExpr e => e -> QueryM QL.Term
expr = fmap snd . exprV

exprV :: ToExpr e => e -> QueryM (MaybeView (ExprIsView e), QL.Term)
exprV e = case toExpr e of
  Expr f -> f
  SpotExpr (Document tbl@(Table _ _ mkey) d) -> do
    ref <- tableRef tbl
    return $ ((,) (ViewOf tbl)) defaultValue { 
      QL.type' = QL.GETBYKEY,
      QL.get_by_key = Just $ QL.GetByKey ref
             (fromMaybe defaultPrimaryAttr $ fmap uFromString mkey)
             (toJsonTerm d) }

-- | Convert a stream to a term
stream :: ToStream a => a -> QueryM QL.Term
stream = expr . toStream

-- | Convert a value to a term
value :: ToValue a => a -> QueryM QL.Term
value = expr . toValue

-- | build a raw protobuf Term
toJsonTerm :: ToJSON a => a -> QL.Term
toJsonTerm a = defaultValue {
  QL.type' = QL.JSON,
  QL.jsonstring = Just $ Utf8 (encode a)
  }

-- | Test if a field is present in a json Value and return it
(.?) :: FromJSON a => Value -> String -> Maybe a
(.?) (Object h) k = toMaybe . fromJSON =<< HM.lookup (T.pack k) h
  where toMaybe (Success a) = Just a
        toMaybe _ = Nothing
(.?) _ _ = Nothing

-- | Helper function to handle responses to a query
whenSuccess :: FromJSON a => String -> (a -> Either String b) -> [Value] -> Either String b
whenSuccess key f response = do
  info <- maybe (Left "invalid response") Right (convert =<< listToMaybe response)
  if info .? "errors" /= Just (0 :: Int)
    then maybe (Left "unknown error") Left $ info .? "first_error"
    else fromMaybe (Left "key missing in response") (fmap f (info .? key))

-- | same as whenSuccess, but ignore the response when there is no error
whenSuccess_ :: b -> [Value] -> Either String b
whenSuccess_ b response = do
  info <- maybe (Left "invalid response") Right (convert =<< listToMaybe response)
  if info .? "errors" /= Just (0 :: Int)
    then maybe (Left "unknown error") Left $ info .? "first_error"
    else Right b

-- | Like aeson's decode, but but works on numbers and strings, not only objects and arrays
decodeAny :: FromJSON a => ByteString -> Maybe a
decodeAny s =
  case Attoparsec.parse Data.Aeson.Parser.value s of
    Attoparsec.Done _ v -> convert v
    _          -> Nothing

-- | Convert a JSON Value into another type
convert :: FromJSON a => Value -> Maybe a
convert v = case fromJSON v of
  Success a -> Just a
  _         -> Nothing

-- | Extract the error message from a Response if there is an error
-- | Help build meta queries
metaQuery :: QueryM QL.MetaQuery -> QueryM QL.Query
metaQuery q = do
  t <- getToken
  mq <- q
  return $ QL.Query QL.META t Nothing Nothing $ Just mq

-- | Convert an int to a 4-byte bytestring
packUInt :: Int -> ByteString
packUInt n = pack $ map fromIntegral $ 
               [n `mod` 256, (n `shiftR` 8) `mod` 256,
                (n `shiftR` 16) `mod` 256, (n `shiftR` 24) `mod` 256]

-- | Convert a 4-bte byestring to an int
unpackUInt :: ByteString -> Int
unpackUInt s = case unpack s of
  [a,b,c,d] -> fromIntegral a .|.
               fromIntegral b `shiftL` 8 .|.
               fromIntegral c `shiftL` 16 .|.
               fromIntegral d `shiftL` 24
  _ -> error "unpackUInt: lengh is not 4"

op :: QL.BuiltinType -> QLBuiltin.Builtin
op o = defaultValue { QLBuiltin.type' = o }

apply :: QL.Builtin -> [QueryM QL.Term] -> QueryM QL.Term
apply o args = do
  a <- sequence args
  return $ defaultValue { QL.type' = QL.CALL, QL.call = Just $ QL.Call o (Seq.fromList a) }

rapply :: [QueryM QL.Term] -> QL.Builtin -> QueryM QL.Term
rapply = flip apply

simpleOp :: ExprIsView (Expr t) ~ False => QL.BuiltinType -> [QueryM QL.Term] -> Expr t
simpleOp o a = mkExpr $ apply (op o) a

withView :: MaybeView b -> QueryM QL.Term -> QueryM (MaybeView b, QL.Term)
withView v = fmap ((,) v)

primaryAttr :: (ToExpr e, ExprTypeIsView (ExprType e) ~ True) =>
               e -> String -> Expr (ExprType e)
primaryAttr e a = Expr $ do
  (ViewOf (Table mdb name _), ex) <- exprV e
  return (ViewOf (Table mdb name (Just a)), ex)

comparison :: ExprTypeIsView t ~ False => QL.Comparison -> [QueryM QL.Term] -> Expr t
comparison o a = mkExpr $ rapply a (op QL.COMPARE) { QL.comparison = Just o }

-- | Build a protobuf TableRef object
tableRef :: Table -> QueryM QL.TableRef
tableRef (Table mdb tb _) = do
  curdb <- activeDB
  useOutdated <- fmap _queryUseOutdated S.get
  return $ QL.TableRef (uFromString $ databaseName $ fromMaybe curdb mdb)
    (uFromString tb) (Just useOutdated)

extractTerm :: ToExpr e => e -> QL.Term
extractTerm e = fst $ runQueryM (expr e) def

dumpExpr :: ToExpr e => e -> String
dumpExpr = dumpTermPart "" . extractTerm

dumpTermPart :: Data a => String -> a -> String
dumpTermPart p a = case dataTypeName (dataTypeOf a) of
  name | "Database.RethinkDB.Internal" `isPrefixOf` name ->
    showConstr (toConstr a) ++ maybeFields a
       | ".Utf8" `isSuffixOf` name ->
         show (uToString (fromJust $ cast a))
       | ".Double" `isSuffixOf` name ->
           show (fromJust $ cast a :: Double)
       | ".Seq" `isSuffixOf` name -> dumpSeq a 
       | otherwise -> dataTypeName (dataTypeOf a) -- showConstr (toConstr a)
  where fieldValues t = gmapQ maybeDump t
        fields t = catMaybes $ zipWith (\x y -> fmap ((x ++ ": ") ++) y)
                   (constrFields (toConstr t)) (fieldValues t)
        maybeFields t = let f = fields t in if null f then ""
                        else " {\n" ++ p ++ concat (intersperse (",\n"++p) f) ++ " }"
        maybeDump :: Data a => a -> Maybe String
        maybeDump t = case showConstr (toConstr t) of
          "Nothing" -> Nothing
          "Just" -> Just $ head (gmapQ (dumpTermPart (p ++ "  ")) t)
          "empty" -> Nothing -- empty Seq
          "ExtField" -> Nothing
          _ -> Just $ dumpTermPart (p ++ "  ") t
        dumpSeq t = let elems :: Data a => a -> [String]
                        elems tt = case showConstr (toConstr tt) of 
                          "empty" -> []
                          _ -> gmapQi 0 (dumpTermPart (p ++ "  ")) tt : gmapQi 1 elems tt
          in "[" ++ concat (intersperse ", " $ elems t) ++ "]"