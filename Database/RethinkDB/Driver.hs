module Database.RethinkDB.Driver where

import Data.Aeson hiding (Result)
import Control.Monad

import Database.RethinkDB.Network
import Database.RethinkDB.Term

run :: Result r => RethinkDBHandle -> Term -> IO r
run h t = do
  let (q, bt) = buildQuery t 0 (rdbDatabase h)
  r <- runQLQuery h q bt
  convertResult r

class Result r where
  convertResult :: MVar Response -> IO r

instance Result Response where
    convertResult r = takeMVar

instance FromJSON a => Result (Cursor a) where
  convertResult r = fmap (unsafe . fromJSON) $ makeCursor r
    where
      unsafe (Data.Aeson.Error e) = error e
      unsafe (Data.Aeson.Success a) = a

instance FromJSON a => Result [a] where
  convertResult = cursorAll <=< convertResult

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
