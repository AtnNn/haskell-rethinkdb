module Database.RethinkDB.Driver where

import Database.RethinkDB.Functions
import Database.RethinkDB.Network
import Database.RethinkDB.Term

run :: Result r => RethinkDBHandle -> Term -> IO r
run h t = withNewToken h $ \token -> do
  q <- buildQuery t token (rdbDatabase h)
  r <- runQLQuery h q
  convertResponse r

class Result r where
  convertResponse :: BaseTerm -> Response -> IO r

class Result Response where
    run _ r = r



{-
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
-}
