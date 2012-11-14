{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, MultiParamTypeClasses, 
             TypeFamilies, ExistentialQuantification, FlexibleInstances, 
             ConstraintKinds #-}

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

data ExprTypeKind = StreamType Bool ValueTypeKind |
                    ValueType ValueTypeKind
data ValueTypeKind = NumberType | BoolType | ObjectType | ArrayType |
                     StringType | NoneType | OtherValueType

type family ExprWritable expr :: Bool
type instance ExprWritable (Expr (StreamType True o)) = True
type instance ExprWritable (Expr (StreamType False o)) = False
type instance ExprWritable (Expr (ValueType v)) = False

type family ExprValueType expr :: ValueTypeKind
type instance ExprValueType (Expr (ValueType v)) = v

data Expr (t :: ExprTypeKind) =
  Expr (Database -> [String] -> QL.Term) |
  SpotExpr Document

class ToExpr o where
  type ExprType o :: ExprTypeKind
  toExpr :: o -> Expr (ExprType o)

instance ToExpr Document where
  type ExprType Document = StreamType True 'ObjectType
  toExpr doc = SpotExpr doc

toTerm :: Expr t -> Database -> [String] -> QL.Term
toTerm (Expr f) curdb vars = f curdb vars
toTerm (SpotExpr (Document tbl@(Table _ _ mkey) d)) curdb _ = defaultValue { 
    QL.type' = QL.GETBYKEY,
    QL.get_by_key = Just $ QL.GetByKey (tableRef curdb tbl)
             (fromMaybe defaultPrimaryAttr $ fmap uFromString mkey)
             (toJsonTerm d)
    }

instance ToExpr Table where
  type ExprType Table = StreamType True ObjectType
  toExpr tbl = Expr $ \curdb _ -> defaultValue { 
    QL.type' = QL.TABLE,
    QL.table = Just $ QL.Table (tableRef curdb tbl)
    }
                                  
instance ToExpr (Expr t) where
  type ExprType (Expr t) = t
  toExpr e = e

defaultPrimaryAttr :: Utf8
defaultPrimaryAttr = uFromString "id"
                    
data Query a = Query (Int64 -> Database -> [String] -> QL.Query) ([Value] -> Either String a)

instance Functor Query where
  fmap f (Query a g)= Query a (fmap f . g)

class ToMapping map where
  type MappingFrom map :: ValueTypeKind
  type MappingTo map :: ValueTypeKind
  toMapping :: map -> Mapping (MappingFrom map) (MappingTo map)

data Mapping (from :: ValueTypeKind) (to :: ValueTypeKind) =
  Mapping (Database -> [String] -> QL.Mapping)

instance ToMapping Value where
  type MappingFrom Value = ObjectType
  type MappingTo Value = ObjectType
  toMapping v = Mapping $ \_ _ -> defaultValue { QLMapping.body = toJsonTerm v }

toQLMapping :: ToMapping m => m -> Database -> [String] -> QL.Mapping
toQLMapping m = case toMapping m of Mapping f -> f

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

tableToTerm :: Table -> Database -> QL.Term
tableToTerm (Table mdb name _) curdb = defaultValue {
  QL.type' = QL.TABLE,
  QL.table = Just $ QL.Table $ QL.TableRef
             (uFromString $ databaseName $ fromMaybe curdb mdb) (uFromString name) Nothing
  }

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

between :: (ToJSON a, ToExpr e, ExprType e ~ StreamType x ObjectType) =>
           (Maybe String) -> (Maybe a) -> (Maybe a) -> e -> Expr (ExprType e)
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

instance ToQuery (Expr t) where
  type QueryType (Expr t) = [Value]
  toQuery (SpotExpr doc) = fmap return $ toQuery doc
  toQuery (Expr f) =
    Query (\token curdb vars -> QL.Query QL.READ token
                           (Just $ defaultValue {
                               QLReadQuery.term = f curdb vars
                               }) Nothing Nothing)
    (Right)

mappingToPredicate :: QL.Mapping -> QL.Predicate
mappingToPredicate (QL.Mapping arg body _1) = defaultValue {
  QLPredicate.arg = arg,
  QLPredicate.body = body
  }

js :: String -> Expr (ValueType any)
js s = Expr $ \_ _ -> defaultValue {
  QL.type' = QL.JAVASCRIPT,
  QL.javascript = Just $ uFromString ("return (" ++ s ++ ")")
  }

bind :: ToExpr e => e -> (Expr (ExprType e) -> Expr tt) -> Expr tt
bind val f = Expr $ \curdb (v:vs) -> let (v1, v2) = splitVars vs in defaultValue {
  QL.type' = QL.LET,
  QL.let' = Just $ QL.Let (Seq.singleton $
                           QL.VarTermTuple (uFromString v) (toTerm (toExpr val) curdb v1))
           (toTerm (toExpr (f $ var v)) curdb v2)
  }

let' :: ToExpr e => String -> e -> Expr t -> Expr t
let' nam val e = Expr $ \curdb vars -> let (v1, v2) = splitVars vars in defaultValue {
  QL.type' = QL.LET,
  QL.let' = Just $ QL.Let (Seq.singleton $
                           QL.VarTermTuple (uFromString nam) (toTerm (toExpr val) curdb v1))
           (toTerm (toExpr e) curdb v2)
  }

var :: String -> Expr t
var v = Expr $ \_ _ -> defaultValue { 
  QL.type' = QL.VAR,
  QLTerm.var = Just $ uFromString v
  }

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

instance (ToExpr b) => ToMapping (Expr (ValueType t) -> b) where 
            type MappingFrom (Expr (ValueType t) -> b) = t
            type MappingTo (Expr (ValueType t) -> b) = ExprValueType b
            toMapping f = Mapping $ \curdb (v:vars) -> defaultValue {
              QLMapping.arg = uFromString v,
              QLMapping.body = toTerm (toExpr (f (var v))) curdb vars
              }

instance ToExpr Int where
  type ExprType Int = ValueType NumberType
  toExpr n = Expr $ \_ _ -> defaultValue {
    QL.type' = QL.NUMBER, QL.number = Just $ fromIntegral n }

instance ToExpr Integer where
  type ExprType Integer = ValueType NumberType
  toExpr n = Expr $ \_ _ -> defaultValue {
    QL.type' = QL.NUMBER, QL.number = Just $ fromInteger n }

instance ToExpr Double where
  type ExprType Double = ValueType NumberType
  toExpr n = Expr $ \_ _ -> defaultValue {
    QL.type' = QL.NUMBER, QL.number = Just n }

instance ToExpr a => ToExpr [a] where
  type ExprType [a] = ValueType ArrayType
  toExpr l = Expr $ \curdb vars -> let vs = splitsVars vars in defaultValue {
    QL.type' = QL.ARRAY, QL.array = Seq.fromList $ 
    zipWith (\x v -> toTerm (toExpr x) curdb v) l vs }

type HasValueType a v = (ToExpr a, ExprType a ~ ValueType v)
type HaveValueType a b v = (HasValueType a v, HasValueType b v)
type NumberExpr = Expr (ValueType NumberType)
type BoolExpr = Expr (ValueType BoolType)
type ObjectExpr = Expr (ValueType ObjectType)
type ArrayExpr = Expr (ValueType ArrayType)
type ValueExpr t = Expr (ValueType t)

slice :: (HasValueType a x, HaveValueType b c y) => a -> b -> c -> ValueExpr x
slice = triOp $ defaultValue { QLBuiltin.type' = QL.SLICE }

append :: (HasValueType a ArrayType, HasValueType b x) => a -> b -> ArrayExpr
append = binOp $ defaultValue { QLBuiltin.type' = QL.ARRAYAPPEND }

-- TODO: wrong?
hasattr :: HasValueType a StringType => a -> BoolExpr
hasattr = unaryOp $ defaultValue { QLBuiltin.type' = QL.IMPLICIT_HASATTR }

-- TODO: wrong
pick :: HasValueType a StringType => [a] -> ObjectExpr
pick = opMany $ defaultValue { QLBuiltin.type' = QL.IMPLICIT_PICKATTRS }

pickFrom :: (HasValueType a ObjectType, HasValueType b StringType) => a -> [b] -> ObjectExpr
pickFrom = opOneMany $ defaultValue { QLBuiltin.type' = QL.PICKATTRS }

concatMap :: (ToExpr e, ExprType e ~ StreamType x from) =>
             (Expr (ValueType from) -> Expr (ValueType ArrayType)) ->
             e -> Expr (StreamType False to)
concatMap fun = unaryOp' $ \curdb vars -> defaultValue {
  QLBuiltin.type' = QL.CONCATMAP,
  QL.concat_map = Just $ QL.ConcatMap $ toQLMapping fun curdb vars
  }

branch :: (ToExpr e, ExprType e ~ (ValueType BoolType),
           ToExpr a, ExprType a ~ x,
           ToExpr b, ExprType b ~ x) =>
          e -> a -> b -> Expr x
branch t a b = Expr $ \curdb vars -> let (v1:v2:v3:_) = splitsVars vars in defaultValue {
  QL.type' = QL.IF, QL.if_ = Just $ QL.If (toTerm (toExpr t) curdb v1)
                                          (toTerm (toExpr a) curdb v2)
                                          (toTerm (toExpr b) curdb v3) }

jsfun :: (ToExpr e, ExprType e ~ ValueType x) => String -> e -> Expr (ValueType y)
jsfun f e = Expr $ \curdb (v:vars) -> toTerm (let' v e $ js $ f ++ "(" ++ v ++ ")") curdb vars 

-- outerJoin :: needs map, concatMap, let, letvar, branch and length
-- innerJoin
-- eqJoin
-- skip
-- limit
-- trim
-- nth
-- pluck
-- without
-- reduce
-- count
-- distinct
-- groupedMapReduce
-- pick
-- unpick
-- merge
-- contains
-- forEach
-- stream_to_array
-- array_to_stream
-- TODO: mapmerge ?