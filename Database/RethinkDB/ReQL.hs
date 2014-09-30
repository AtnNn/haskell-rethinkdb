{-# LANGUAGE ExistentialQuantification, RecordWildCards, ScopedTypeVariables,
             FlexibleInstances, OverloadedStrings, PatternGuards, GADTs #-}

-- | Building RQL queries in Haskell
module Database.RethinkDB.ReQL (
  ReQL(..),
  op, op',
  BaseReQL(..),
  BaseAttribute(..),
  OptArg(..),
  buildQuery,
  BaseArray,
  Backtrace, convertBacktrace,
  Expr(..),
  QuerySettings(..),
  newVarId,
  str,
  num,
  Attribute(..),
  cons,
  arr,
  baseArray,
  withQuerySettings,
  Object(..),
  obj,
  returnVals,
  nonAtomic,
  canReturnVals,
  canNonAtomic,
  reqlToProtobuf,
  Bound(..),
  closedOrOpen,
  datumTerm,
  baseBool,
  nil
  ) where

import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as M
import Data.Maybe (fromMaybe, catMaybes)
import Data.String (IsString(..))
import Data.List (intersperse)
import qualified Data.Sequence as S
import Control.Monad.State (State, get, put, runState)
import Control.Applicative ((<$>))
import Data.Default (Default, def)
import qualified Data.Text as T
import qualified Data.Aeson as J
import Data.Foldable (toList)
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad.Fix

import Database.RethinkDB.Objects as O

-- | An RQL term
data ReQL = ReQL { baseReQL :: State QuerySettings BaseReQL }

data BaseReQL = BaseReQL {
    termType :: TermType,
    termDatum :: Maybe Datum.Datum,
    termArgs :: BaseArray,
    termOptArgs :: [BaseAttribute]
} deriving Eq

data QuerySettings = QuerySettings {
  queryToken :: Int64,
  queryDefaultDatabase :: Database,
  queryVarIndex :: Int,
  queryUseOutdated :: Maybe Bool,
  queryReturnVals :: Maybe Bool,
  queryAtomic :: Maybe Bool
  }

instance Default QuerySettings where
  def = QuerySettings 0 (Database "") 0 Nothing Nothing Nothing

withQuerySettings :: (QuerySettings -> ReQL) -> ReQL
withQuerySettings f = ReQL $ (baseReQL . f) =<< get

-- | Include the value of single write operations in the returned object
returnVals :: ReQL -> ReQL
returnVals (ReQL t) = ReQL $ do
  state <- get
  put state{ queryReturnVals = Just True }
  ret <- t
  state' <- get
  put state'{ queryReturnVals = queryReturnVals state }
  return ret

-- | Allow non-atomic replace
nonAtomic :: ReQL -> ReQL
nonAtomic (ReQL t) = ReQL $ do
  state <- get
  put state{ queryAtomic = Just False }
  ret <- t
  state' <- get
  put state'{ queryAtomic = queryAtomic state }
  return ret

canReturnVals :: ReQL -> ReQL
canReturnVals (ReQL t) = ReQL $ t >>= \ret -> do
  qs <- get
  case queryReturnVals qs of
    Nothing -> return ret
    Just rv -> return ret{
      termOptArgs = BaseAttribute "return_vals" (baseBool rv) : termOptArgs ret }

canNonAtomic :: ReQL -> ReQL
canNonAtomic (ReQL t) = ReQL $ t >>= \ret -> do
  qs <- get
  case queryAtomic qs of
    Nothing -> return ret
    Just atomic -> return ret{
      termOptArgs = BaseAttribute "non_atomic" (baseBool $ not atomic) : termOptArgs ret }

baseBool :: Bool -> BaseReQL
baseBool b = BaseReQL DATUM (Just defaultValue{
                                Datum.type' = Just R_BOOL, r_bool = Just b }) [] []

newVarId :: State QuerySettings Int
newVarId = do
  QuerySettings {..} <- get
  let n = queryVarIndex + 1
  put QuerySettings {queryVarIndex = n, ..}
  return $ n

instance Show BaseReQL where
  show (BaseReQL DATUM (Just dat) _ _) = showD dat
  show (BaseReQL MAKE_ARRAY _ x []) = "[" ++ (concat $ intersperse ", " $ map show x) ++ "]"
  show (BaseReQL MAKE_OBJ _ [] x) = "{" ++ (concat $ intersperse ", " $ map show x) ++ "}"
  show (BaseReQL MAKE_OBJ _ args []) = "{" ++ (concat $ intersperse ", " $ map (\(a,b) -> show a ++ ":" ++ show b) $ pairs args) ++ "}"
     where pairs (a:b:xs) = (a,b) : pairs xs
           pairs _ = []
  show (BaseReQL VAR _ [BaseReQL DATUM (Just d) [] []] []) | Just x <- toDouble d =
    "x" ++ show (round x :: Int)
  show (BaseReQL FUNC _ [BaseReQL DATUM (Just d) [] [], body] []) | Just vars <- toDoubles d =
    "(\\" ++ (concat $ intersperse " " $ map (("x"++) . show . (round :: Double -> Int)) $ vars)
    ++ " -> " ++ show body ++ ")"
  show (BaseReQL GET_FIELD _ [o, k] []) = show o ++ "!" ++ show k
  show (BaseReQL fun _ args optargs) =
    show fun ++ "(" ++
    concat (intersperse ", " (map show args ++ map show optargs)) ++ ")"

showD :: Datum.Datum -> String
showD d = case Datum.type' d of
  Just R_NUM -> show' $ r_num d
  Just R_BOOL -> show' $ r_bool d
  Just R_STR -> show' $ r_str d
  Just R_ARRAY -> "[" ++ (concat $ intersperse ", " $ map showD $ toList $ r_array d) ++ "]"
  Just R_OBJECT ->
    "{" ++ (concat $ intersperse ", " $ map showDatumAttr $ toList $ r_object d) ++ "}"
  Just R_NULL -> "null"
  Just R_JSON -> "(JSON)" ++ show' (r_str d)
  Nothing -> "Nothing"
  where show' Nothing = "Nothing"
        show' (Just a) = show a

showDatumAttr:: Datum.AssocPair -> String
showDatumAttr (Datum.AssocPair (Just k) (Just v)) = uToString k ++ ": " ++ showD v
showDatumAttr x = show x

-- | Convert other types into ReqL expressions
class Expr e where
  expr :: e -> ReQL

instance Expr ReQL where
  expr t = t

-- | A list of terms
data Array = Array { baseArray :: State QuerySettings BaseArray }

type BaseArray = [BaseReQL]

-- | Build arrays of exprs
class Arr a where
  arr :: a -> Array

cons :: Expr e => e -> Array -> Array
cons x xs = Array $ do
  bt <- baseReQL (expr x)
  xs' <- baseArray xs
  return $ bt : xs'

instance Arr () where
  arr () = Array $ return []

instance Expr a => Arr [a] where
  arr [] = Array $ return []
  arr (x:xs) = cons x (arr xs)

instance (Expr a, Expr b) => Arr (a, b) where
  arr (a,b) = cons a $ cons b $ arr ()

instance (Expr a, Expr b, Expr c) => Arr (a, b, c) where
  arr (a,b,c) = cons a $ cons b $ cons c $ arr ()

instance (Expr a, Expr b, Expr c, Expr d) => Arr (a, b, c, d) where
  arr (a,b,c,d) = cons a $ cons b $ cons c $ cons d $ arr ()

instance Arr Array where
  arr = id

-- | A list of key/value pairs
data Object = Object { objectAttributes :: [Attribute] }

instance Show Object where
  show = show . expr

infix 0 :=

-- | A key/value pair used for building objects
data Attribute where 
  (:=) :: Expr e => T.Text -> e -> Attribute
  (::=) :: (Expr k, Expr v) => k -> v -> Attribute

data BaseAttribute = BaseAttribute T.Text BaseReQL deriving Eq

data OptArg = forall e . Expr e => T.Text :== e

mapBaseAttribute :: (BaseReQL -> BaseReQL) -> BaseAttribute -> BaseAttribute
mapBaseAttribute f (BaseAttribute k v) = BaseAttribute k (f v)

instance Show BaseAttribute where
  show (BaseAttribute a b) = T.unpack a ++ ": " ++ show b

-- | Convert a list of attributes into a ReQL object
obj :: [Attribute] -> Object
obj = Object

baseOptArgs :: [OptArg] -> State QuerySettings [BaseAttribute]
baseOptArgs = sequence . map toBase
  where toBase (k :== v) = BaseAttribute k <$> baseReQL (expr v)

-- | Build a term
op' :: Arr a => TermType -> a -> [OptArg] -> ReQL
op' t a b = ReQL $ do
  a' <- baseArray (arr a)
  b' <- baseOptArgs b
  case (t, a') of
    (FUNCALL, (BaseReQL FUNC Nothing [argsFunDatum, fun] [] : argsCall)) |
      BaseReQL DATUM (Just argsFunArray) [] [] <- argsFunDatum,
      Just varsFun <- toDoubles argsFunArray,
      length varsFun == length argsCall,
      Just varsCall <- varsOf argsCall ->
        return $ alphaRename (zip varsFun varsCall) fun
    _ -> return $ BaseReQL t Nothing a' b'

-- | Build a term with no optargs
op :: Arr a => TermType -> a -> ReQL
op t a = op' t a []

toDoubles :: Datum.Datum -> Maybe [Double]
toDoubles Datum.Datum{
  Datum.type' = Just R_ARRAY,
  r_array = s } =
  sequence . map toDouble . toList $ s
toDoubles _ = Nothing

toDouble :: Datum.Datum -> Maybe Double
toDouble Datum.Datum{ type' = Just R_NUM, r_num = Just n } = Just n
toDouble _ = Nothing

varsOf :: [BaseReQL] -> Maybe [Double]
varsOf = sequence . map varOf
    
varOf :: BaseReQL -> Maybe Double
varOf (BaseReQL VAR Nothing [BaseReQL DATUM (Just d) [] []] []) = toDouble d
varOf _ = Nothing

datumNumberArray :: [Int] -> Datum.Datum
datumNumberArray a =
  defaultValue{
    Datum.type' = Just R_ARRAY,
    r_array = S.fromList $ map datumInt a }

datumInt :: Int -> Datum.Datum
datumInt n =
  defaultValue{
    Datum.type' = Just R_NUM,
    r_num = Just $ fromIntegral n }

alphaRename :: [(Double, Double)] -> BaseReQL -> BaseReQL
alphaRename assoc = fix $ \f x ->
  case varOf x of
    Just n
      | Just n' <- lookup n assoc ->
      BaseReQL VAR Nothing
      [BaseReQL DATUM
       (Just $ defaultValue{ Datum.type' = Just R_NUM, r_num = Just n' }) [] []] []
      | otherwise -> x
    _ -> updateChildren x f

updateChildren :: BaseReQL -> (BaseReQL -> BaseReQL) -> BaseReQL
updateChildren (BaseReQL t d a o) f = BaseReQL t d (map f a) (map (mapBaseAttribute f) o)

datumTerm :: DatumType -> Datum.Datum -> ReQL
datumTerm t d = ReQL $ return $ BaseReQL DATUM (Just d { Datum.type' = Just t }) [] []

-- | A shortcut for inserting strings into ReQL expressions
-- Useful when OverloadedStrings makes the type ambiguous
str :: String -> ReQL
str s = datumTerm R_STR defaultValue { r_str = Just (uFromString s) }

-- | A shortcut for inserting numbers into ReQL expressions
num :: Double -> ReQL
num = expr

instance Expr Int64 where
  expr i = datumTerm R_NUM defaultValue { r_num = Just (fromIntegral i) }

instance Expr Int where
  expr i = datumTerm R_NUM defaultValue { r_num = Just (fromIntegral i) }

instance Expr Integer where
  expr i = datumTerm R_NUM defaultValue { r_num = Just (fromIntegral i) }

instance Num ReQL where
  fromInteger x = datumTerm R_NUM defaultValue { r_num = Just (fromInteger x) }
  a + b = op ADD (a, b)
  a * b = op MUL (a, b)
  a - b = op SUB (a, b)
  negate a = op SUB (0 :: Double, a)
  abs n = op BRANCH (op TermType.LT (n, 0 :: Double), negate n, n)
  signum n = op BRANCH (op TermType.LT (n, 0 :: Double),
                        -1 :: Double,
                        op BRANCH (op TermType.EQ (n, 0 :: Double), 0 :: Double, 1 :: Double))
instance Expr T.Text where
  expr t = datumTerm R_STR defaultValue { r_str = Just (uFromString $ T.unpack t) }

instance Expr Bool where
  expr b = datumTerm R_BOOL defaultValue { r_bool = Just b }

instance Expr () where
  expr _ = datumTerm R_NULL defaultValue

instance IsString ReQL where
  fromString s = datumTerm R_STR defaultValue { r_str = Just (uFromString $ s) }

instance (a ~ ReQL) => Expr (a -> ReQL) where
  expr f = ReQL $ do
    v <- newVarId
    baseReQL $ op FUNC (datumNumberArray [v], expr $ f (op VAR [v]))

instance (a ~ ReQL, b ~ ReQL) => Expr (a -> b -> ReQL) where
  expr f = ReQL $ do
    a <- newVarId
    b <- newVarId
    baseReQL $ op FUNC (datumNumberArray [a, b], expr $ f (op VAR [a]) (op VAR [b]))

instance Expr Datum.Datum where
  expr d = ReQL $ return $ BaseReQL DATUM (Just d) [] []

instance Expr Table where
  expr (Table mdb name _) = withQuerySettings $ \QuerySettings {..} ->
    op' TABLE (fromMaybe queryDefaultDatabase mdb, name) $ catMaybes [
      fmap ("use_outdated" :==) queryUseOutdated ]

instance Expr Database where
  expr (Database name) = op DB [name]

instance Expr J.Value where
  expr J.Null = expr ()
  expr (J.Bool b) = expr b
  expr (J.Number n) = expr (fromRational (toRational n) :: Double)
  expr (J.String t) = expr t
  expr (J.Array a) = expr a
  expr (J.Object o) = expr o

instance Expr Double where
  expr d = datumTerm R_NUM defaultValue { r_num = Just d }

instance Expr Rational where
  expr x = expr (fromRational x :: Double)

instance Expr x => Expr (V.Vector x) where
  expr v = expr (V.toList v)

instance Expr a => Expr [a] where
  expr a = expr $ arr a

instance Expr Array where
  expr a = op MAKE_ARRAY a

instance Expr e => Expr (M.HashMap T.Text e) where
  expr m = expr $ obj $ map (uncurry (:=)) $ M.toList m

instance Expr Object where
  expr (Object attrs) =
    if all knownKey attrs
    then op' MAKE_OBJ () $ map toOptArg attrs
    else op OBJECT $ concatMap pairs attrs
    where pairs (k := v) = [expr k, expr v]
          pairs (k ::= v) = [expr k, expr v]
          knownKey (_ := _) = True
          knownKey (_ ::= _) = False
          toOptArg (k := v) = k :== v
          toOptArg (_ ::= _) = error "unreachable"

buildBaseReQL :: BaseReQL -> Term
buildBaseReQL BaseReQL {..} = defaultValue {
    Term.type' = Just termType,
    datum = termDatum,
    args = buildBaseArray termArgs,
    optargs = buildTermAssoc termOptArgs }

buildBaseArray :: BaseArray -> Seq Term
buildBaseArray [] = S.empty
buildBaseArray (x:xs) = buildBaseReQL x S.<| buildBaseArray xs

buildTermAssoc :: [BaseAttribute] -> Seq AssocPair
buildTermAssoc = S.fromList . map buildTermAttribute

buildTermAttribute :: BaseAttribute -> AssocPair
buildTermAttribute (BaseAttribute k v) = AssocPair (Just $ uFromString $ T.unpack k) (Just $ buildBaseReQL v)

buildQuery :: ReQL -> Int64 -> Database -> (Query.Query, BaseReQL)
buildQuery term token db = (defaultValue {
                              Query.type' = Just START,
                              Query.query = Just pterm },
                            bterm)
  where bterm =
         fst $ runState (baseReQL term) (def {queryToken = token,
                                              queryDefaultDatabase = db })
        pterm = buildBaseReQL bterm

instance Show ReQL where
  show t = show . snd $ buildQuery t 0 (Database "")

reqlToProtobuf :: ReQL -> Query.Query
reqlToProtobuf t = fst $ buildQuery t 0 (Database "")

type Backtrace = [Frame]

data Frame = FramePos Int64 | FrameOpt T.Text

instance Show Frame where
    show (FramePos n) = show n
    show (FrameOpt k) = show k

convertBacktrace :: QL.Backtrace -> Backtrace
convertBacktrace = concatMap convertFrame . toList . QL.frames
    where convertFrame QL.Frame { type' = Just QL.POS, pos = Just n } = [FramePos n]
          convertFrame QL.Frame { type' = Just QL.OPT, opt = Just k } = [FrameOpt (T.pack $ uToString k)]
          convertFrame _ = []

instance Expr UTCTime where
  expr t = op EPOCH_TIME [expr . toRational $ utcTimeToPOSIXSeconds t]

instance Expr ZonedTime where
  expr (ZonedTime
        (LocalTime
         date
         (TimeOfDay hour minute seconds))
        timezone) = let
    (year, month, day) = toGregorian date
    in  op TIME [
      expr year, expr month, expr day, expr hour, expr minute, expr (toRational seconds),
      str $ timeZoneOffsetString timezone]

instance Expr BaseReQL where
  expr = ReQL . return

-- | An upper or lower bound for between and during
data Bound a =
  Open { getBound :: a } -- ^ An inclusive bound
  | Closed { getBound :: a } -- ^ An exclusive bound

closedOrOpen :: Bound a -> T.Text
closedOrOpen Open{} = "open"
closedOrOpen Closed{} = "closed"

nil :: ReQL
nil = expr ([] :: [()])

instance (Expr a, Expr b) => Expr (a, b) where
  expr (a, b) = expr [expr a, expr b]

instance (Expr a, Expr b, Expr c) => Expr (a, b, c) where
  expr (a, b, c) = expr [expr a, expr b, expr c]

instance (Expr a, Expr b, Expr c, Expr d) => Expr (a, b, c, d) where
  expr (a, b, c, d) = expr [expr a, expr b, expr c, expr d]

instance (Expr a, Expr b, Expr c, Expr d, Expr e) => Expr (a, b, c, d, e) where
  expr (a, b, c, d, e) = expr [expr a, expr b, expr c, expr d, expr e]
