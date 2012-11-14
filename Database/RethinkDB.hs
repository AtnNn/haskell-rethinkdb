{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, MultiParamTypeClasses, 
             TypeFamilies, ExistentialQuantification #-}

module Database.RethinkDB where

import qualified Database.RethinkDB.Internal.Types as QL
import qualified Database.RethinkDB.Internal.Query_Language.Response as QLResponse
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

data RethinkDBHandle = RethinkDBHandle {
  rdbHandle :: Handle, 
  rdbToken :: IORef Int64,
  rdbDatabase :: Database
  }

data Database = Database {
  databaseName :: String
  } deriving (Eq, Ord, Show)

openConnection :: HostName -> Maybe PortID -> Maybe String -> IO RethinkDBHandle
openConnection host port mdb = do
  h <- connectTo host (fromMaybe (PortNumber 28015) port)
  hPut h initialMessage
  r <- newIORef 1
  db' <- maybe (fmap head $ run (RethinkDBHandle h r (Database "")) $ dbList) (return . Database) mdb
  return (RethinkDBHandle h r db')
  where initialMessage = packUInt 0xaf61ba35

use :: RethinkDBHandle -> Database -> RethinkDBHandle
use h db' = h { rdbDatabase = db' }

run :: ToQuery a => RethinkDBHandle -> a -> IO (QueryType a)
run h q = do
  r <- runEither h q
  case r of
    Left e -> error e
    Right a -> return a

runEither :: ToQuery a => RethinkDBHandle -> a -> IO (Either String (QueryType a))
runEither h q = do
  let Query f g = toQuery q
  er <- runQLQuery h (\x -> f x (rdbDatabase h) newVars)
  return $ er >>= \r -> (responseErrorMessage r >>
    (maybe (Left "decode error") Right . sequence . map (decodeAny . utf8) $
     toList $ QL.response r) >>= g)

runMaybe :: ToQuery a => RethinkDBHandle -> a -> IO (Maybe (QueryType a))
runMaybe h = fmap (either (const Nothing) Just) . runEither h

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
                             QL.type' = QL.TABLE, 
                             QL.table = Just $ QL.Table $ tableRef curdb tbl
                             }
                         }
                     ) Nothing Nothing)
    Right

uTableKey :: Table -> Utf8
uTableKey (Table _ _ mkey) = fromMaybe defaultPrimaryAttr $ fmap uFromString mkey

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

packUInt :: Int -> ByteString
packUInt n = pack $ map fromIntegral $ 
               [n `mod` 256, (n `shiftR` 8) `mod` 256,
                (n `shiftR` 16) `mod` 256, (n `shiftR` 24) `mod` 256]

unpackUInt :: ByteString -> Int
unpackUInt s = case unpack s of
  [a,b,c,d] -> fromIntegral a .|.
               fromIntegral b `shiftL` 8 .|.
               fromIntegral c `shiftL` 16 .|.
               fromIntegral d `shiftL` 24
  _ -> error "unpackUInt: lengh is not 4"
               

closeConnection :: RethinkDBHandle -> IO ()
closeConnection (RethinkDBHandle h _ _) = hClose h

recvAll :: RethinkDBHandle -> Int -> IO ByteString
recvAll (RethinkDBHandle h _ _) n = hGet h n

sendAll :: RethinkDBHandle -> ByteString -> IO ()
sendAll (RethinkDBHandle h _ _) s = hPut h s

getToken :: RethinkDBHandle -> IO Int64
getToken (RethinkDBHandle _ r _) = atomicModifyIORef r $ \t -> (t + 1, t)

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

dbCreate :: String -> Query Database
dbCreate db_name = Query
  (metaQuery $ \_ _ -> 
    QL.MetaQuery QL.CREATE_DB (Just $ uFromString db_name) Nothing Nothing)
  (const $ Right $ Database db_name)

dbDrop :: Database -> Query ()
dbDrop (Database name) = Query
  (metaQuery $ \_ _ -> QL.MetaQuery QL.DROP_DB (Just $ uFromString name) Nothing Nothing)
  (const $ Right ())

db :: String -> Database
db s = Database s

responseErrorMessage :: QL.Response -> Either String ()
responseErrorMessage response = 
  if QL.status_code response `elem` [QL.SUCCESS_EMPTY, QL.SUCCESS_JSON,
                                     QL.SUCCESS_PARTIAL, QL.SUCCESS_STREAM]
    then Right ()
    else Left $ maybe (show $ QL.status_code response) uToString $ QL.error_message response

metaQuery :: (Database -> [String] -> QL.MetaQuery) -> Int64 -> Database -> [String] -> QL.Query
metaQuery q t d v = QL.Query QL.META t Nothing Nothing $ Just (q d v)

dbList :: Query [Database]
dbList = Query
  (metaQuery $ \_ _ -> QL.MetaQuery QL.LIST_DBS Nothing Nothing Nothing)
  (maybe (Left "error") Right . sequence . map (fmap Database . convert))
  
decodeAny :: FromJSON a => ByteString -> Maybe a
decodeAny s =
  case Attoparsec.parse value s of
    Attoparsec.Done _ v -> convert v
    _          -> Nothing

convert :: FromJSON a => Value -> Maybe a
convert v = case fromJSON v of
  Success a -> Just a
  _         -> Nothing

data TableCreateOptions = TableCreateOptions {
  tableDataCenter :: Maybe String,
  tableCacheSize :: Maybe Int64
  }

instance Default TableCreateOptions where
  def = TableCreateOptions Nothing Nothing

data Table = Table {
  tableDatabase :: Maybe Database,
  tableName :: String,
  tablePrimaryAttr :: Maybe String
  } deriving (Show, Eq, Ord)

table :: String -> Table
table n = Table Nothing n Nothing

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

tableDrop :: Table -> Query ()
tableDrop (Table tdb tb _) = Query
  (metaQuery $ \curdb _ -> 
    QL.MetaQuery QL.DROP_TABLE Nothing Nothing $ Just $
    QL.TableRef (uFromString $ databaseName $ fromMaybe curdb tdb) (uFromString tb) Nothing)
  (const $ Right ())

tableList :: Database -> Query [Table]
tableList (Database name) = Query 
  (metaQuery $ \_ _ -> 
    QL.MetaQuery QL.LIST_TABLES (Just $ uFromString name) Nothing Nothing)
  (maybe (Left "error") Right . sequence .
   map (fmap (\x -> Table (Just (Database name)) x Nothing) . convert))

data Document = Document {
  documentTable :: Table,
  documentKey :: Value
  } deriving Show

tableRef :: Database -> Table -> QL.TableRef
tableRef curdb (Table mdb tb _) =
  QL.TableRef (uFromString $ databaseName $ fromMaybe curdb mdb) (uFromString tb) Nothing

insert_or_upsert :: ToJSON a => Table -> [a] -> Bool -> Query [Document]
insert_or_upsert tbl as overwrite = Query
  (\token curdb _ -> let write = defaultValue {
          QLWriteQuery.type' = QL.INSERT,
          QL.insert = Just $ QL.Insert (tableRef curdb tbl)
                      (Seq.fromList $ map toJsonTerm as) (Just overwrite)
          } in QL.Query QL.WRITE token Nothing (Just write) Nothing)
  (whenSuccess "generated_keys" $ \keys -> Right $ map (\doc -> Document tbl doc) keys)

whenSuccess :: FromJSON a => String -> (a -> Either String b) -> [Value] -> Either String b
whenSuccess key f response = do
  info <- maybe (Left "invalid response") Right (convert =<< listToMaybe response)
  if info .? "errors" /= Just (0 :: Int)
    then maybe (Left "unknown error") Left $ info .? "first_error"
    else fromMaybe (Left "key missing in response") (fmap f (info .? key))

whenSuccess_ :: b -> [Value] -> Either String b
whenSuccess_ b response = do
  info <- maybe (Left "invalid response") Right (convert =<< listToMaybe response)
  if info .? "errors" /= Just (0 :: Int)
    then maybe (Left "unknown error") Left $ info .? "first_error"
    else Right b

insert :: ToJSON a => Table -> a -> Query Document
insert tb a = fmap head $ insert_or_upsert tb [a] False

insertMany :: ToJSON a => Table -> [a] -> Query [Document]
insertMany tb a = insert_or_upsert tb a False

upsert :: ToJSON a => Table -> a -> Query Document
upsert tb a = fmap head $ insert_or_upsert tb [a] True

upsertMany :: ToJSON a => Table -> [a] -> Query [Document]
upsertMany tb a = insert_or_upsert tb a True

(.?) :: FromJSON a => Value -> String -> Maybe a
(.?) (Object h) k = toMaybe . fromJSON =<< HM.lookup (T.pack k) h
  where toMaybe (Success a) = Just a
        toMaybe _ = Nothing
(.?) _ _ = Nothing

toJsonTerm :: ToJSON a => a -> QL.Term
toJsonTerm a = defaultValue {
  QL.type' = QL.JSON,
  QL.jsonstring = Just $ Utf8 (encode a)
  }

data ExprTypeKind = SequenceType ValueTypeKind | ValueType ValueTypeKind
data ValueTypeKind = NumberType | BoolType | ObjectType | OtherTermType

data Expr (write :: Bool) (t :: ExprTypeKind) =
  Expr (Database -> [String] -> QL.Term) |
  SpotExpr Document

class ToExpr o where
  type ExprWritable o :: Bool
  type ExprType o :: ExprTypeKind
  toExpr :: o -> Expr (ExprWritable o) (ExprType o)

instance ToExpr Document where
  type ExprWritable Document = True
  type ExprType Document = SequenceType 'ObjectType
  toExpr doc = SpotExpr doc

toTerm :: Expr w t -> Database -> [String] -> QL.Term
toTerm (Expr f) curdb vars = f curdb vars
toTerm (SpotExpr (Document tbl@(Table _ _ mkey) d)) curdb _ = defaultValue { 
    QL.type' = QL.GETBYKEY,
    QL.get_by_key = Just $ QL.GetByKey (tableRef curdb tbl)
             (fromMaybe defaultPrimaryAttr $ fmap uFromString mkey)
             (toJsonTerm d)
    }

instance ToExpr Table where
  type ExprWritable Table = True
  type ExprType Table = SequenceType ObjectType
  toExpr tbl = Expr $ \curdb _ -> defaultValue { 
    QL.type' = QL.TABLE,
    QL.table = Just $ QL.Table (tableRef curdb tbl)
    }
                                  
instance ToExpr (Expr w t) where
  type ExprWritable (Expr w t) = w
  type ExprType (Expr w t) = t
  toExpr e = e

defaultPrimaryAttr :: Utf8
defaultPrimaryAttr = uFromString "id"
                    
data Query a = Query (Int64 -> Database -> [String] -> QL.Query) ([Value] -> Either String a)

instance Functor Query where
  fmap f (Query a g)= Query a (fmap f . g)

class Mapping map where
  toQLMapping :: map -> QL.Mapping

instance Mapping Value where
  toQLMapping v = defaultValue { QLMapping.body = toJsonTerm v }

update :: (ToExpr sel, ExprWritable sel ~ True, Mapping map) => sel -> map -> Query ()
update view m = Query
  (\token curdb _ ->
    QL.Query QL.WRITE token Nothing (Just $ write curdb) Nothing)
  (whenSuccess_ ())

  where write curdb = case toExpr view of
          Expr f -> defaultValue {
            QLWriteQuery.type' = QL.UPDATE,
            QL.update = Just $ QL.Update (f curdb newVars) (toQLMapping m)
            }
          SpotExpr (Document tbl@(Table _ _ k) d) -> defaultValue {
            QLWriteQuery.type' = QL.POINTUPDATE,
            QL.point_update = Just $ QL.PointUpdate (tableRef curdb tbl)
                              (fromMaybe defaultPrimaryAttr $ fmap uFromString k)
                              (toJsonTerm d)
                              (toQLMapping m)
            }

tableToTerm :: Table -> Database -> QL.Term
tableToTerm (Table mdb name _) curdb = defaultValue {
  QL.type' = QL.TABLE,
  QL.table = Just $ QL.Table $ QL.TableRef
             (uFromString $ databaseName $ fromMaybe curdb mdb) (uFromString name) Nothing
  }

replace :: (ToExpr sel, ExprWritable sel ~ True, ToJSON a) => sel -> a -> Query ()
replace view a = Query
  (\token curdb _ ->
    QL.Query QL.WRITE token Nothing (Just $ write curdb) Nothing)
  (whenSuccess_ ())

  where write curdb = case toExpr view of
          Expr f -> defaultValue {
            QLWriteQuery.type' = QL.MUTATE,
            QL.mutate = Just $ QL.Mutate (f curdb newVars) (toQLMapping $ toJSON a)
            }
          SpotExpr (Document tbl@(Table _ _ k) d) -> defaultValue {
            QLWriteQuery.type' = QL.POINTMUTATE,
            QL.point_mutate = Just $ QL.PointMutate (tableRef curdb tbl)
                              (fromMaybe defaultPrimaryAttr $ fmap uFromString k)
                              (toJsonTerm d)
                              (toQLMapping $ toJSON a)
            }

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

get :: ToJSON a => Table -> a -> Document
get t = Document t . toJSON

between :: (ToJSON a, ToExpr e, ExprType e ~ SequenceType x) =>
           (Maybe String) -> (Maybe a) -> (Maybe a) -> e -> Expr (ExprWritable e) (ExprType e)
between k a b e = Expr $ \curdb vars -> defaultValue {
     QL.type' = QL.CALL,
     QL.call = Just $ QL.Call {
       QL.builtin = defaultValue {
          QLBuiltin.type' = QL.RANGE,
          QL.range = Just $ QL.Range (fromMaybe defaultPrimaryAttr $ fmap uFromString k)
                     (fmap toJsonTerm a) (fmap toJsonTerm b)
          },
       QL.args = Seq.singleton $ toTerm (toExpr e) curdb vars
       }
     }

instance ToQuery (Expr w t) where
  type QueryType (Expr w t) = [Value]
  toQuery (SpotExpr doc) = fmap return $ toQuery doc
  toQuery (Expr f) =
    Query (\token curdb vars -> QL.Query QL.READ token
                           (Just $ defaultValue {
                               QLReadQuery.term = f curdb vars
                               }) Nothing Nothing)
    (Right)

filter :: (ToExpr e, ExprType e ~ SequenceType x,
           Mapping fil) =>
           fil -> e -> Expr (ExprWritable e) (ExprType e)
filter fil e = Expr $ \curdb vars -> defaultValue {
     QL.type' = QL.CALL,
     QL.call = Just $ QL.Call {
       QL.builtin = defaultValue {
          QLBuiltin.type' = QL.FILTER,
          QL.filter = Just $ QL.Filter $ mappingToPredicate $ toQLMapping fil
          },
       QL.args = Seq.singleton $ toTerm (toExpr e) curdb vars
       }
     }

mappingToPredicate :: QL.Mapping -> QL.Predicate
mappingToPredicate (QL.Mapping arg body _1) = defaultValue {
  QLPredicate.arg = arg,
  QLPredicate.body = body
  }

js :: String -> Expr False any
js s = Expr $ \_ _ -> defaultValue {
  QL.type' = QL.JAVASCRIPT,
  QL.javascript = Just $ uFromString ("return (" ++ s ++ ")")
  }

bind :: ToExpr e => e -> (Expr (ExprWritable e) (ExprType e) -> Expr ww tt) -> Expr ww tt
bind val f = Expr $ \curdb (v:vs) -> let (v1, v2) = splitVars vs in defaultValue {
  QL.type' = QL.LET,
  QL.let' = Just $ QL.Let (Seq.singleton $
                           QL.VarTermTuple (uFromString v) (toTerm (toExpr val) curdb v1))
           (toTerm (toExpr (f $ var v)) curdb v2)
  }

let' :: ToExpr e => String -> e -> Expr w t -> Expr w t
let' nam val e = Expr $ \curdb vars -> let (v1, v2) = splitVars vars in defaultValue {
  QL.type' = QL.LET,
  QL.let' = Just $ QL.Let (Seq.singleton $
                           QL.VarTermTuple (uFromString nam) (toTerm (toExpr val) curdb v1))
           (toTerm (toExpr e) curdb v2)
  }

var :: String -> Expr w t
var v = Expr $ \_ _ -> defaultValue { 
  QL.type' = QL.VAR,
  QLTerm.var = Just $ uFromString v
  }

newVars :: [String]
newVars = concat $ zipWith (\n as -> map (show n ++) as) [0 :: Int ..] (map (map return) $ repeat ['a'..'z'])

splitVars :: [String] -> ([String], [String])
splitVars vs = (map ('a':) vs, map ('b':) vs)

-- outerJoin :: needs map, concatMap, let, letvar, branch and length
-- innerJoin
-- eqJoin
-- zip :: needs branch map lambda contains merge
-- map
-- concatMap
-- orderBy
-- skip
-- limit
-- trim
-- nth
-- pluck
-- without
-- union
-- reduce
-- count
-- distinct
-- groupedMapReduce
-- groupBy (count, sum, avg)
-- pick
-- unpick
-- merge
-- append
-- contains
-- +, -, *, /, %, &, |, ==, !=, >, >=, <, <=, ~
-- branch
-- forEach
-- error
-- stream_to_array
-- array_to_stream
