{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, MultiParamTypeClasses, 
             TypeFamilies, ExistentialQuantification, FlexibleInstances, 
             ConstraintKinds, FlexibleContexts, UndecidableInstances, 
             PolyKinds, FunctionalDependencies, GADTs #-}

-- | The core of the haskell client library for RethinkDB

module Database.RethinkDB.Driver where

import {-# SOURCE #-} Database.RethinkDB.Functions
import Database.RethinkDB.Types

import Data.Typeable
import Data.Data
import GHC.Prim

import qualified Database.RethinkDB.Internal.Types as QL
import qualified Database.RethinkDB.Internal.Query_Language.Response as QLResponse
import qualified Database.RethinkDB.Internal.Query_Language.VarTermTuple as QLVarTermTuple
import qualified Database.RethinkDB.Internal.Query_Language.Term as QLTerm
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

-- | A connection to the database
data RethinkDBHandle = RethinkDBHandle {
  rdbHandle :: Handle, 
  rdbToken :: IORef Int64, -- ^ The next token to use
  rdbDatabase :: Database  -- ^ When no database is specified, this one will be used
  }

-- | Connect to a RethinkDB server
-- 
-- h <- openConnection "localhost" Nothing Nothing

openConnection :: HostName -> Maybe PortID -> Maybe String -> IO RethinkDBHandle
openConnection host port mdb = do
  h <- connectTo host (fromMaybe (PortNumber 28015) port)
  hPut h initialMessage
  r <- newIORef 1
  -- db' <- maybe (fmap head $ run (RethinkDBHandle h r (Database "")) $ dbList) (return . Database) mdb
  let db' = Database $ fromMaybe "" mdb
  return (RethinkDBHandle h r db')
  where initialMessage = packUInt 0xaf61ba35

-- | Change the default database
-- 
-- let h' = h `use` (db "players")

use :: RethinkDBHandle -> Database -> RethinkDBHandle
use h db' = h { rdbDatabase = db' }

-- | Disconnect
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

-- | Execute a raw protobuffer query and return the raw response
runQLQuery :: RethinkDBHandle -> (Int64 -> QL.Query) -> IO (Either String QL.Response)
runQLQuery h query = do
  token <- getNewToken h
  let queryS = messagePut (query token)
  sendAll h $ packUInt (fromIntegral $ B.length queryS) <> queryS
  readResponse token
  
  where readResponse t = do
          header <- recvAll h 4
          responseS <- recvAll h (unpackUInt header)
          let eResponse = messageGet responseS
          case eResponse of
            Left errMsg -> return $ Left errMsg
            Right (response, rest)
              | B.null rest ->
                (case QLResponse.token response of
                  n | n == t -> return $ Right response
                    | n > t -> return $ Left "RethinkDB: runQLQuery: invalid response token"
                    | otherwise -> readResponse t)
              | otherwise ->
                  return $ Left $ "RethinkDB: runQLQuery: invalid reply length"

-- * CRUD

-- | A database, referenced by name
data Database = Database {
  databaseName :: String
  } deriving (Eq, Ord, Show)

-- | A short function to create a Database reference
db :: String -> Database
db s = Database s

-- | Create a database on the server
dbCreate :: String -> Query Database
dbCreate db_name = Query
  (metaQuery $ return $ QL.MetaQuery QL.CREATE_DB (Just $ uFromString db_name) Nothing Nothing)
  (const $ Right $ Database db_name)

-- | Drop a database
dbDrop :: Database -> Query ()
dbDrop (Database name) = Query
  (metaQuery $ return $ QL.MetaQuery QL.DROP_DB (Just $ uFromString name) Nothing Nothing)
  (const $ Right ())

-- | List the databases on the server
dbList :: Query [Database]
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

-- | A table
data Table = Table {
  tableDatabase :: Maybe Database, -- ^ when nothing, use the rdbDatabase
  tableName :: String,
  tablePrimaryAttr :: Maybe String -- ^ when nothing, "id" is used
  } deriving (Show, Eq, Ord)

-- | "id"
defaultPrimaryAttr :: Utf8
defaultPrimaryAttr = uFromString "id"

-- | Create a simple table refence with no associated database or primary key
-- 
-- table "music"
-- 
-- Another way to create table references is to use the Table constructor:
-- 
-- Table (Just "mydatabase") "music" (Just "tuneid")

table :: String -> Table
table n = Table Nothing n Nothing

-- | Create a table on the server
-- 
-- t <- run h $ tableCreate (table "fruits") def

tableCreate :: Table -> TableCreateOptions -> Query Table
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
               (const $ Right $ Table Nothing table_name primary_key)

-- | Drop a table
tableDrop :: Table -> Query ()
tableDrop tbl = Query
  (metaQuery $ do
      ref <- tableRef tbl
      return $ QL.MetaQuery QL.DROP_TABLE Nothing Nothing $ Just $ ref)
  (const $ Right ())

-- | List the tables in a database
tableList :: Database -> Query [Table]
tableList (Database name) = Query 
  (metaQuery $ return $ 
    QL.MetaQuery QL.LIST_TABLES (Just $ uFromString name) Nothing Nothing)
  (maybe (Left "error") Right . sequence .
   map (fmap (\x -> Table (Just (Database name)) x Nothing) . convert))

-- | Get the primary key of the table as a Utf8, or "id" if there is none
uTableKey :: Table -> Utf8
uTableKey (Table _ _ mkey) = fromMaybe defaultPrimaryAttr $ fmap uFromString mkey

-- | A document
data Document = Document {
  documentTable :: Table,
  documentKey :: Value
  } deriving Show

-- | Get a document by its primary key
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

insert_or_upsert :: ToJSON a => Table -> [a] -> Bool -> Query [Document]
insert_or_upsert tbl as overwrite = Query
  (do token <- getToken
      ref <- tableRef tbl
      let write = defaultValue {
          QLWriteQuery.type' = QL.INSERT,
          QL.insert = Just $ QL.Insert ref
                      (Seq.fromList $ map toJsonTerm as) (Just overwrite) }
      return $ QL.Query QL.WRITE token Nothing (Just write) Nothing)
  (whenSuccess "generated_keys" $ \keys -> Right $ map (\doc -> Document tbl doc) keys)

-- | Insert a document into a table
-- 
-- d <- run h $ insert t (object ["name" .= "banane", "color" .= "red"])

insert :: ToJSON a => Table -> a -> Query Document
insert tb a = fmap head $ insert_or_upsert tb [a] False

-- | Like insert, but for multiple documents at once
insertMany :: ToJSON a => Table -> [a] -> Query [Document]
insertMany tb a = insert_or_upsert tb a False

-- | Like insert, but overwrite any existing document with the same primary key
-- 
-- The primary key defined in the Table reference is ignored for this operation

upsert :: ToJSON a => Table -> a -> Query Document
upsert tb a = fmap head $ insert_or_upsert tb [a] True

-- | Like upsert, but for a list of values
upsertMany :: ToJSON a => Table -> [a] -> Query [Document]
upsertMany tb a = insert_or_upsert tb a True

-- | Update a table
-- 
-- t <- run h $ tableCreate (table "example") def
-- run h $ insertMany t [object ["a" .= 1, "b" .= 11], object ["a" .= 2, "b" .= 12]]
-- run h $ update t (object ["b" .= 20])
-- run h $ t

update :: (ToExpr sel, ExprType sel ~ StreamType True out, ToMapping map,
           MappingFrom map ~ out, MappingTo map ~ ObjectType) =>
          sel -> map -> Query ()
update view m = Query
  (do token <- getToken 
      mT <- mapping m
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
      return $ QL.Query QL.WRITE token Nothing (Just write) Nothing)
  (whenSuccess_ ())

-- | Replace the objects return by a query by another object
replace :: (ToExpr sel, ExprIsView sel ~ True, ToJSON a) => sel -> a -> Query ()
replace view a = Query
  (do token <- getToken 
      fun <- mapping (toJSON a)
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
      return $ QL.Query QL.WRITE token Nothing (Just write) Nothing)
  (whenSuccess_ ())

-- | Delete the result of a query from the database
delete :: (ToExpr sel, ExprIsView sel ~ True) => sel -> Query ()
delete view = Query
  (do token <- getToken 
      write <- case toExpr view of
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
      return $ QL.Query QL.WRITE token Nothing (Just write) Nothing)
  (whenSuccess_ ())

-- * Queries

-- | A query returning a
data Query a = Query { 
  _queryBuild :: QueryM QL.Query,
  _queryExtract :: [Value] -> Either String a
  }

instance Functor Query where
  fmap f (Query a g)= Query a (fmap f . g)

-- | Convert things like tables and documents into queries
class ToQuery a where
  type QueryType a
  toQuery :: a -> Query (QueryType a)

instance ToQuery (Query a) where
  type QueryType (Query a) = a
  toQuery a = a

instance ToQuery Table where
  type QueryType Table = [Value]
  toQuery tbl = Query
    (do token <- getToken 
        ref <- tableRef tbl
        return $ QL.Query QL.READ token
                     (Just $ defaultValue {
                         QLReadQuery.term = defaultValue {
                             QLTerm.type' = QL.TABLE, 
                             QLTerm.table = Just $ QL.Table ref
                             }
                         }
                     ) Nothing Nothing)
    Right

instance ToQuery Document where
  type QueryType Document = Value
  toQuery (Document tbl d) = Query
    (do token <- getToken 
        ref <- tableRef tbl
        return $ QL.Query QL.READ token
                     (Just $ defaultValue {
                         QLReadQuery.term = defaultValue {
                             QL.type' = QL.GETBYKEY, 
                             QL.get_by_key = Just $ QL.GetByKey ref
                                             (uTableKey tbl) (toJsonTerm d) } }
                     ) Nothing Nothing)
    (maybe (Left "insufficient results") Right . listToMaybe)

instance ToQuery (Expr t) where
  type QueryType (Expr t) = [Value]
  toQuery (SpotExpr doc) = fmap return $ toQuery doc
  toQuery (Expr f) =
    Query (do token <- getToken
              (_, ex) <- f
              return $ QL.Query QL.READ token
                (Just $ defaultValue {
                    QLReadQuery.term = ex
                    }) Nothing Nothing)
    Right

-- | Submit a query to the server, throwing exceptions when there is an error
run :: ToQuery a => RethinkDBHandle -> a -> IO (QueryType a)
run h q = do
  r <- runEither h q
  case r of
    Left e -> error e
    Right a -> return a

-- | Return a (Left String) if there is an error
runEither :: ToQuery a => RethinkDBHandle -> a -> IO (Either String (QueryType a))
runEither h q = do
  let Query f g = toQuery q
  er <- runQLQuery h (\x -> fst $ runQueryM f $ QuerySettings x (rdbDatabase h) initialVars False)
  return $ er >>= \r -> (responseErrorMessage r >>
    (maybe (Left "decode error") Right . sequence . map (decodeAny . utf8) $
     toList $ QL.response r) >>= g)

-- | Return Nothing when there is an error
runMaybe :: ToQuery a => RethinkDBHandle -> a -> IO (Maybe (QueryType a))
runMaybe h = fmap (either (const Nothing) Just) . runEither h

-- | Get the raw response from a query
runRaw :: ToQuery q => RethinkDBHandle -> q -> IO (Either String QL.Response)
runRaw h q =
  runQLQuery h (\tok -> fst $ runQueryM (_queryBuild (toQuery q)) $ QuerySettings tok (rdbDatabase h) initialVars False)

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
  Expr :: QueryM (MaybeView (ExprIsView (Expr t)), QL.Term) -> Expr t -- ^ A protobuf term
  SpotExpr :: Document -> Expr (StreamType True ObjectType)           -- ^ A single document

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
  toValue = streamToValue

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
  toValue = streamToValue
                                  
instance ToStream Table where
  toStream = toExpr

instance ToExpr (Expr t) where
  type ExprType (Expr t) = t
  toExpr e = e

instance ToValue (Expr (ValueType t)) where
  toValue e = e

instance ToValue (Expr (StreamType w t)) where
  toValue = streamToValue

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
            ex <- expr a
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
type ObjectStream b = Expr (StreamType b ObjectType)
type ViewExpr = ObjectStream True

-- | What values can be compared with <, <=, > and >=
class CanCompare (a :: ValueTypeKind)
instance CanCompare NumberType
instance CanCompare StringType

instance Num (Expr (ValueType NumberType)) where
  (+) = plus
  (-) = minus
  (*) = times
  abs = jsfun "abs"
  signum = signum'
  fromInteger = toExpr

instance Fractional (Expr (ValueType NumberType)) where
  fromRational n = toExpr (fromRational n :: Double)
  (/) = divide

-- | A sequence is either a stream or an array
class Sequence (e :: ExprTypeKind) where
  type SequenceType e (t :: ValueTypeKind) :: Constraint

instance Sequence (StreamType w t) where
  type SequenceType (StreamType w t) tt = t ~ tt

instance a ~ ArrayType => Sequence (ValueType a) where
  type SequenceType (ValueType a) t = ()

-- | A list of String/Expr pairs
data Obj = Obj [Attribute]
data Attribute = forall e . (ToExpr e) => String := e

-- | Build an Obj
obj :: [Attribute] -> Obj
obj = Obj

-- | Convert a stream into a value
-- 
-- The whole stream is read into the server's memory

streamToValue :: (ToExpr e, ExprType e ~ StreamType w t) => e -> Expr (ValueType ArrayType)
streamToValue = simpleOp QL.STREAMTOARRAY . return . expr

-- | Convert a value into a stream
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
initialVars = concat $ zipWith (\n as -> map (++ show n) as) [0 :: Int ..] (map (map return) $ repeat ['a'..'z'])

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
responseErrorMessage :: QL.Response -> Either String ()
responseErrorMessage response = 
  if QL.status_code response `elem` [QL.SUCCESS_EMPTY, QL.SUCCESS_JSON,
                                     QL.SUCCESS_PARTIAL, QL.SUCCESS_STREAM]
    then Right ()
    else Left $ maybe (show $ QL.status_code response) uToString $ QL.error_message response

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