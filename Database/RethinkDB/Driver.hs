{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, MultiParamTypeClasses, 
             TypeFamilies, ExistentialQuantification, FlexibleInstances, 
             ConstraintKinds, FlexibleContexts #-}

-- | The core of the haskell client library for RethinkDB

module Database.RethinkDB.Driver where

import {-# SOURCE #-} Database.RethinkDB.Functions
import Database.RethinkDB.Types

import GHC.Prim as GHC

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

import qualified Data.HashMap.Strict as HM
import Data.Default
import qualified Data.Attoparsec.Lazy as Attoparsec
import Data.Foldable (toList)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Parser (value)
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
getToken :: RethinkDBHandle -> IO Int64
getToken (RethinkDBHandle _ r _) = atomicModifyIORef r $ \t -> (t + 1, t)

-- | Execute a raw protobuffer query and return the raw response
runQLQuery :: RethinkDBHandle -> (Int64 -> QL.Query) -> IO (Either String QL.Response)
runQLQuery h query = do
  token <- getToken h
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
  (metaQuery $ \_ _ -> 
    QL.MetaQuery QL.CREATE_DB (Just $ uFromString db_name) Nothing Nothing)
  (const $ Right $ Database db_name)

-- | Drop a database
dbDrop :: Database -> Query ()
dbDrop (Database name) = Query
  (metaQuery $ \_ _ -> QL.MetaQuery QL.DROP_DB (Just $ uFromString name) Nothing Nothing)
  (const $ Right ())

-- | List the databases on the server
dbList :: Query [Database]
dbList = Query
  (metaQuery $ \_ _ -> QL.MetaQuery QL.LIST_DBS Nothing Nothing Nothing)
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
  (metaQuery $ \curdb _ -> let create = defaultValue {
        QLCreateTable.datacenter = fmap uFromString datacenter,
        QLCreateTable.table_ref = QL.TableRef (uFromString $ databaseName $ fromMaybe curdb mdb)
                                  (uFromString table_name) Nothing, 
        QLCreateTable.primary_key = fmap uFromString primary_key,
        QLCreateTable.cache_size = cache_size
        } in
    QL.MetaQuery QL.CREATE_TABLE Nothing (Just create) Nothing)
    (const $ Right $ Table Nothing table_name primary_key)

-- | Drop a table
tableDrop :: Table -> Query ()
tableDrop (Table tdb tb _) = Query
  (metaQuery $ \curdb _ -> 
    QL.MetaQuery QL.DROP_TABLE Nothing Nothing $ Just $
    QL.TableRef (uFromString $ databaseName $ fromMaybe curdb tdb) (uFromString tb) Nothing)
  (const $ Right ())

-- | List the tables in a database
tableList :: Database -> Query [Table]
tableList (Database name) = Query 
  (metaQuery $ \_ _ -> 
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

-- | Create a reference to a document, given its table and id
get :: ToJSON a => Table -> a -> Document
get t = Document t . toJSON

insert_or_upsert :: ToJSON a => Table -> [a] -> Bool -> Query [Document]
insert_or_upsert tbl as overwrite = Query
  (\token curdb _ -> let write = defaultValue {
          QLWriteQuery.type' = QL.INSERT,
          QL.insert = Just $ QL.Insert (tableRef curdb tbl)
                      (Seq.fromList $ map toJsonTerm as) (Just overwrite)
          } in QL.Query QL.WRITE token Nothing (Just write) Nothing)
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
  (\token curdb vars ->
    QL.Query QL.WRITE token Nothing (Just $ write curdb vars) Nothing)
  (whenSuccess_ ())

  where write curdb vars = let (v1, v2) = splitVars vars in case toExpr view of
          Expr f -> defaultValue {
            QLWriteQuery.type' = QL.UPDATE,
            QL.update = Just $ QL.Update (f curdb newVars) (toQLMapping m curdb v1)
            }
          SpotExpr (Document tbl@(Table _ _ k) d) -> defaultValue {
            QLWriteQuery.type' = QL.POINTUPDATE,
            QL.point_update = Just $ QL.PointUpdate (tableRef curdb tbl)
                              (fromMaybe defaultPrimaryAttr $ fmap uFromString k)
                              (toJsonTerm d)
                              (toQLMapping m curdb v2)
            }

-- | Replace the objects return by a query by another object
replace :: (ToExpr sel, ExprWritable sel ~ True, ToJSON a) => sel -> a -> Query ()
replace view a = Query
  (\token curdb vars ->
    QL.Query QL.WRITE token Nothing (Just $ write curdb vars) Nothing)
  (whenSuccess_ ())

  where write curdb vars = let (v1, v2) = splitVars vars in case toExpr view of
          Expr f -> defaultValue {
            QLWriteQuery.type' = QL.MUTATE,
            QL.mutate = Just $ QL.Mutate (f curdb newVars) (toQLMapping (toJSON a) curdb v1)
            }
          SpotExpr (Document tbl@(Table _ _ k) d) -> defaultValue {
            QLWriteQuery.type' = QL.POINTMUTATE,
            QL.point_mutate = Just $ QL.PointMutate (tableRef curdb tbl)
                              (fromMaybe defaultPrimaryAttr $ fmap uFromString k)
                              (toJsonTerm d)
                              (toQLMapping (toJSON a) curdb v2)
            }

-- | Delete the result of a query from the database
delete :: (ToExpr sel, ExprWritable sel ~ True) => sel -> Query ()
delete view = Query
  (\token curdb _ -> QL.Query QL.WRITE token Nothing (Just $ write curdb) Nothing)
  (whenSuccess_ ())
  
  where write curdb = case toExpr view of
          Expr f -> defaultValue {
            QLWriteQuery.type' = QL.DELETE,
            QL.delete = Just $ QL.Delete (f curdb newVars)
            }
          SpotExpr (Document tbl@(Table _ _ k) d) -> defaultValue {
            QLWriteQuery.type' = QL.POINTDELETE,
            QL.point_delete = Just $ QL.PointDelete (tableRef curdb tbl)
                              (fromMaybe defaultPrimaryAttr $ fmap uFromString k)
                              (toJsonTerm d)
            }

-- * Queries

-- | A query returning a
data Query a = Query { 
  _queryBuild :: Int64 -> Database -> [String] -> QL.Query,
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
    (\token curdb _ -> QL.Query QL.READ token
                     (Just $ defaultValue {
                         QLReadQuery.term = defaultValue {
                             QLTerm.type' = QL.TABLE, 
                             QLTerm.table = Just $ QL.Table $ tableRef curdb tbl
                             }
                         }
                     ) Nothing Nothing)
    Right

instance ToQuery Document where
  type QueryType Document = Value
  toQuery (Document tbl d) = Query
    (\token curdb _ -> QL.Query QL.READ token
                     (Just $ defaultValue {
                         QLReadQuery.term = defaultValue {
                             QL.type' = QL.GETBYKEY, 
                             QL.get_by_key = Just $ QL.GetByKey (tableRef curdb tbl)
                                             (uTableKey tbl) (toJsonTerm d)
                             }
                         }
                     ) Nothing Nothing)
    (maybe (Left "insufficient results") Right . listToMaybe)

instance ToQuery (Expr t) where
  type QueryType (Expr t) = [Value]
  toQuery (SpotExpr doc) = fmap return $ toQuery doc
  toQuery (Expr f) =
    Query (\token curdb vars -> QL.Query QL.READ token
                           (Just $ defaultValue {
                               QLReadQuery.term = f curdb vars
                               }) Nothing Nothing)
    (Right)

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
  er <- runQLQuery h (\x -> f x (rdbDatabase h) newVars)
  return $ er >>= \r -> (responseErrorMessage r >>
    (maybe (Left "decode error") Right . sequence . map (decodeAny . utf8) $
     toList $ QL.response r) >>= g)

-- | Return Nothing when there is an error
runMaybe :: ToQuery a => RethinkDBHandle -> a -> IO (Maybe (QueryType a))
runMaybe h = fmap (either (const Nothing) Just) . runEither h


-- * Expressions

-- | Can the Expr be written to? (updated or deleted)
type family ExprWritable expr :: Bool
type instance ExprWritable (Expr (StreamType True o)) = True
type instance ExprWritable (Expr (StreamType False o)) = False
type instance ExprWritable (Expr (ValueType v)) = False

-- | The type od the value of an Expr
type family ExprValueType expr :: ValueTypeKind
type instance ExprValueType (Expr (ValueType v)) = v

-- | An RQL expression
data Expr (t :: ExprTypeKind) =
    Expr (Database -> [String] -> QL.Term) -- ^ A protobuf term
  | SpotExpr Document                      -- ^ A single document

-- | Convert something into an Expr
class ToExpr (o :: *) where
  type ExprType o :: ExprTypeKind
  toExpr :: o -> Expr (ExprType o)

-- | The result type of toValue
type family TypeToValue (t :: ExprTypeKind) :: ValueTypeKind
type instance TypeToValue (StreamType w t) = ArrayType
type instance TypeToValue (ValueType t) = t

-- | Convert something into a value Expr
class ToValue e where
  toValue :: e -> Expr (ValueType (TypeToValue (ExprType e)))

instance ToExpr Document where
  type ExprType Document = StreamType True 'ObjectType
  toExpr doc = SpotExpr doc

instance ToValue Document where
  toValue = streamToValue

instance ToExpr Table where
  type ExprType Table = StreamType True ObjectType
  toExpr tbl = Expr $ \curdb _ -> defaultValue { 
    QL.type' = QL.TABLE,
    QL.table = Just $ QL.Table (tableRef curdb tbl)
    }

instance ToValue Table where
  toValue = streamToValue
                                  
instance ToExpr (Expr t) where
  type ExprType (Expr t) = t
  toExpr e = e

instance ToValue (Expr (ValueType t)) where
  toValue e = e

instance ToValue (Expr (StreamType w t)) where
  toValue = streamToValue

instance ToExpr Int where
  type ExprType Int = ValueType NumberType
  toExpr n = Expr $ \_ _ -> defaultValue {
    QL.type' = QL.NUMBER, QL.number = Just $ fromIntegral n }

instance ToValue Int where
  toValue = toExpr

instance ToExpr Integer where
  type ExprType Integer = ValueType NumberType
  toExpr n = Expr $ \_ _ -> defaultValue {
    QL.type' = QL.NUMBER, QL.number = Just $ fromInteger n }

instance ToValue Integer where
  toValue = toExpr

instance ToExpr Double where
  type ExprType Double = ValueType NumberType
  toExpr n = Expr $ \_ _ -> defaultValue {
    QL.type' = QL.NUMBER, QL.number = Just n }

instance ToValue Double where
  toValue = toExpr

instance ToExpr a => ToExpr [a] where
  type ExprType [a] = ValueType ArrayType
  toExpr l = Expr $ \curdb vars -> let vs = splitsVars vars in defaultValue {
    QL.type' = QL.ARRAY, QL.array = Seq.fromList $ 
    zipWith (\x v -> toTerm (toExpr x) curdb v) l vs }

instance ToExpr a => ToValue [a] where
  toValue = toExpr

instance ToExpr () where
  type ExprType () = ValueType NoneType
  toExpr () = Expr $ \_ _ -> defaultValue { QL.type' = QL.JSON_NULL }

instance ToValue () where
  toValue = toExpr

instance ToExpr Obj where
  type ExprType Obj = ValueType ObjectType
  toExpr (Obj o) = Expr $ \curdb vars -> let vs = splitsVars vars in defaultValue {
    QL.type' = QL.OBJECT, QL.object = Seq.fromList $ zipWith (go curdb) o vs }
    where go curdb (k := a) v = QL.VarTermTuple {
            QLVarTermTuple.var = uFromString k, QLVarTermTuple.term = toTerm (toExpr a) curdb v}

instance ToValue Obj where
  toValue = toExpr

-- | Aliases for type constraints on expressions
type HasValueType a v = (ToExpr a, ExprType a ~ ValueType v)
type HaveValueType a b v = (HasValueType a v, HasValueType b v)

-- | Simple aliases for a value Expr
type NumberExpr = Expr (ValueType NumberType)
type BoolExpr = Expr (ValueType BoolType)
type ObjectExpr = Expr (ValueType ObjectType)
type ArrayExpr = Expr (ValueType ArrayType)
type StringExpr = Expr (ValueType StringType)
type ValueExpr t = Expr (ValueType t)

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
  type SequenceType e :: ValueTypeKind

instance Sequence (StreamType w t) where
  type SequenceType (StreamType w t) = t

instance a ~ ArrayType => Sequence (ValueType a) where
  type SequenceType (ValueType a) = GHC.Any

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
streamToValue = (unaryOp $ defaultValue { QLBuiltin.type' = QL.STREAMTOARRAY }) . toExpr

-- | Convert a value into a stream
valueToStream :: (ToExpr e, ExprType e ~ ValueType ArrayType) => e -> Expr (StreamType False t)
valueToStream = (unaryOp $ defaultValue { QLBuiltin.type' = QL.STREAMTOARRAY }) . toExpr

-- * Mappings

-- | A mapping is a like single-parameter function
data Mapping (from :: ValueTypeKind) (to :: ValueTypeKind) =
  Mapping (Database -> [String] -> QL.Mapping)

-- | Convert objects into mappings
class ToMapping map where
  type MappingFrom map :: ValueTypeKind
  type MappingTo map :: ValueTypeKind
  toMapping :: map -> Mapping (MappingFrom map) (MappingTo map)

instance ToMapping Obj where
  type MappingFrom Obj = ObjectType
  type MappingTo Obj = ObjectType
  toMapping v = Mapping $ \curdb vars -> defaultValue {
    QLMapping.body = toTerm (toExpr v) curdb vars }

instance ToMapping Value where
  type MappingFrom Value = ObjectType
  type MappingTo Value = ObjectType
  toMapping v = Mapping $ \_ _ -> defaultValue { QLMapping.body = toJsonTerm v }

instance (ToExpr b) => ToMapping (Expr (ValueType t) -> b) where 
            type MappingFrom (Expr (ValueType t) -> b) = t
            type MappingTo (Expr (ValueType t) -> b) = ExprValueType b
            toMapping f = Mapping $ \curdb (v:vars) -> defaultValue {
              QLMapping.arg = uFromString v,
              QLMapping.body = toTerm (toExpr (f (var v))) curdb vars
              }

-- * Utilities

-- | Convert a protobuf Mapping into a Predicate
mappingToPredicate :: QL.Mapping -> QL.Predicate
mappingToPredicate (QL.Mapping arg body _1) = defaultValue {
  QLPredicate.arg = arg,
  QLPredicate.body = body
  }

-- | Convert a table to a raw protobuf term
tableToTerm :: Table -> Database -> QL.Term
tableToTerm (Table mdb name _) curdb = defaultValue {
  QL.type' = QL.TABLE,
  QL.table = Just $ QL.Table $ QL.TableRef
             (uFromString $ databaseName $ fromMaybe curdb mdb) (uFromString name) Nothing
  }

-- | Convert into a raw protobuf mapping
toQLMapping :: ToMapping m => m -> Database -> [String] -> QL.Mapping
toQLMapping m = case toMapping m of Mapping f -> f

-- | Convert an Expr to a term
toTerm :: Expr t -> Database -> [String] -> QL.Term
toTerm (Expr f) curdb vars = f curdb vars
toTerm (SpotExpr (Document tbl@(Table _ _ mkey) d)) curdb _ = defaultValue { 
    QL.type' = QL.GETBYKEY,
    QL.get_by_key = Just $ QL.GetByKey (tableRef curdb tbl)
             (fromMaybe defaultPrimaryAttr $ fmap uFromString mkey)
             (toJsonTerm d)
    }

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

-- | Build a protobuf TableRef object
tableRef :: Database -> Table -> QL.TableRef
tableRef curdb (Table mdb tb _) =
  QL.TableRef (uFromString $ databaseName $ fromMaybe curdb mdb) (uFromString tb) Nothing

-- | Like aeson's decode, but but works on numbers and strings, not only objects and arrays
decodeAny :: FromJSON a => ByteString -> Maybe a
decodeAny s =
  case Attoparsec.parse value s of
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
metaQuery :: (Database -> [String] -> QL.MetaQuery) -> Int64 -> Database -> [String] -> QL.Query
metaQuery q t d v = QL.Query QL.META t Nothing Nothing $ Just (q d v)

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

newVars :: [String]
newVars = concat $ zipWith (\n as -> map (show n ++) as) [0 :: Int ..] (map (map return) $ repeat ['a'..'z'])

splitVars :: [String] -> ([String], [String])
splitVars vs = (map ('a':) vs, map ('b':) vs)

splitsVars :: [String] -> [[String]]
splitsVars vs = zipWith (\p v -> map (p++) v) newVars (repeat vs)

binOp :: (ToExpr a, ToExpr b) =>
         QL.Builtin -> a -> b -> Expr c
binOp op a b = Expr $ \curdb vars -> let (v1, v2) = splitVars vars in defaultValue {
  QL.type' = QL.CALL,
  QL.call = Just $ QL.Call op $ Seq.fromList [toTerm (toExpr a) curdb v1,
                                              toTerm (toExpr b) curdb v2]
  }

triOp :: (ToExpr a, ToExpr b, ToExpr c) =>
         QL.Builtin -> a -> b -> c -> Expr d
triOp op a b c = Expr $ \curdb vars -> let (v1:v2:v3:_) = splitsVars vars in defaultValue {
  QL.type' = QL.CALL,
  QL.call = Just $ QL.Call op $ Seq.fromList [toTerm (toExpr a) curdb v1,
                                              toTerm (toExpr b) curdb v2,
                                              toTerm (toExpr c) curdb v3]
  }

unaryOp :: ToExpr a => QL.Builtin -> a -> Expr b
unaryOp op a = Expr $ \curdb vars -> defaultValue {
  QL.type' = QL.CALL,
  QL.call = Just $ QL.Call op $ Seq.fromList [toTerm (toExpr a) curdb vars] 
  }

unaryOp' :: ToExpr a => (Database -> [String] -> QL.Builtin) -> a -> Expr b
unaryOp' op a = Expr $ \curdb vars -> let (v1, v2) = splitVars vars in defaultValue {
  QL.type' = QL.CALL,
  QL.call = Just $ QL.Call (op curdb v1) $ Seq.fromList [toTerm (toExpr a) curdb v2] 
  }

zeroOp :: (Database -> [String] -> QL.Builtin) -> Expr b
zeroOp op = Expr $ \curdb vars -> defaultValue {
  QL.type' = QL.CALL,
  QL.call = Just $ QL.Call (op curdb vars) $ Seq.fromList []
  }

comparison :: (ToExpr a, ExprType a ~ ValueType x,
               ToExpr b, ExprType b ~ ValueType y) =>
         QL.Comparison -> a -> b -> Expr (ValueType BoolType)
comparison comp = binOp $ defaultValue { QLBuiltin.type' = QL.COMPARE,
                                         QL.comparison = Just comp }

opMany :: ToExpr a => 
         QL.Builtin -> [a] -> Expr c
opMany op as = Expr $ \curdb vars -> let vs = splitsVars vars in defaultValue {
  QL.type' = QL.CALL,
  QL.call = Just $ QL.Call op $ Seq.fromList $ zipWith (\x v -> toTerm (toExpr x) curdb v) as vs
  }

opOneMany :: (ToExpr a, ToExpr b) => QL.Builtin -> a -> [b] -> Expr c
opOneMany op a bs = Expr $ \curdb vars -> let (v:vs) = splitsVars vars in defaultValue {
  QL.type' = QL.CALL,
  QL.call = Just $ QL.Call op $ Seq.fromList $ (toTerm (toExpr a) curdb v) : 
            zipWith (\x v' -> toTerm (toExpr x) curdb v') bs vs
  }