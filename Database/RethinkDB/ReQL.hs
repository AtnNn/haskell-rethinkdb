{-# LANGUAGE ExistentialQuantification, RecordWildCards, ScopedTypeVariables,
             FlexibleInstances, OverloadedStrings, PatternGuards, GADTs #-}

-- | Building RQL queries in Haskell
module Database.RethinkDB.ReQL (
  ReQL(..),
  op, op',
  Term(..),
  TermAttribute(..),
  OptArg(..),
  buildQuery,
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
  reqlToJSON,
  Bound(..),
  closedOrOpen,
  datumTerm,
  boolToTerm,
  nil,
  WireQuery(..),
  WireBacktrace(..)
  ) where

import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as M
import Data.Maybe (fromMaybe, catMaybes)
import Data.String (IsString(..))
import Data.List (intersperse)
import Control.Monad.State (State, get, put, runState)
import Control.Applicative ((<$>))
import Data.Default (Default, def)
import qualified Data.Text as T
import qualified Data.Aeson as J
import Data.Aeson (toJSON, Value)
import Data.Foldable (toList)
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad.Fix
import Data.Int
import Data.Monoid
import Data.Scientific

import Database.RethinkDB.Wire
import Database.RethinkDB.Wire.Query
import Database.RethinkDB.Wire.Term as Term
import Database.RethinkDB.Objects

-- | A ReQL term
data ReQL = ReQL { runReQL :: State QuerySettings Term }

data Term = Term {
  termType :: TermType,
  termArgs :: [Term],
  termOptArgs :: [TermAttribute]
  } | Datum {
  termDatum :: Datum
  } deriving Eq

newtype WireTerm = WireTerm { termJSON :: Value }

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
withQuerySettings f = ReQL $ (runReQL . f) =<< get

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
      termOptArgs = TermAttribute "return_vals" (boolToTerm rv) : termOptArgs ret }

canNonAtomic :: ReQL -> ReQL
canNonAtomic (ReQL t) = ReQL $ t >>= \ret -> do
  qs <- get
  case queryAtomic qs of
    Nothing -> return ret
    Just atomic -> return ret{
      termOptArgs = TermAttribute "non_atomic" (boolToTerm $ not atomic) : termOptArgs ret }

boolToTerm :: Bool -> Term
boolToTerm b = Datum $ J.Bool b

newVarId :: State QuerySettings Int
newVarId = do
  QuerySettings {..} <- get
  let n = queryVarIndex + 1
  put QuerySettings {queryVarIndex = n, ..}
  return $ n

instance Show Term where
  show (Datum dat) = show dat
  show (Term MAKE_ARRAY x []) = "[" ++ (concat $ intersperse ", " $ map show x) ++ "]"
  show (Term MAKE_OBJ [] x) = "{" ++ (concat $ intersperse ", " $ map show x) ++ "}"
  show (Term MAKE_OBJ args []) = "{" ++ (concat $ intersperse ", " $ map (\(a,b) -> show a ++ ":" ++ show b) $ pairs args) ++ "}"
     where pairs (a:b:xs) = (a,b) : pairs xs
           pairs _ = []
  show (Term VAR [Datum d] []) | Just x <- toDouble d =
    "x" ++ show (round x :: Int)
  show (Term FUNC [Datum d, body] []) | Just vars <- toDoubles d =
    "(\\" ++ (concat $ intersperse " " $ map (("x"++) . show . (round :: Double -> Int)) $ vars)
    ++ " -> " ++ show body ++ ")"
  show (Term GET_FIELD [o, k] []) = show o ++ "!" ++ show k
  show (Term fun args optargs) =
    show fun ++ "(" ++
    concat (intersperse ", " (map show args ++ map show optargs)) ++ ")"

-- | Convert other types into ReqL expressions
class Expr e where
  expr :: e -> ReQL

instance Expr ReQL where
  expr t = t

-- | A list of terms
data Array = Array { baseArray :: State QuerySettings [Term] }

-- | Build arrays of exprs
class Arr a where
  arr :: a -> Array

cons :: Expr e => e -> Array -> Array
cons x xs = Array $ do
  bt <- runReQL (expr x)
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

data TermAttribute = TermAttribute T.Text Term deriving Eq

data OptArg = forall e . Expr e => T.Text :== e

mapTermAttribute :: (Term -> Term) -> TermAttribute -> TermAttribute
mapTermAttribute f (TermAttribute k v) = TermAttribute k (f v)

instance Show TermAttribute where
  show (TermAttribute a b) = T.unpack a ++ ": " ++ show b

-- | Convert a list of attributes into a ReQL object
obj :: [Attribute] -> Object
obj = Object

baseOptArgs :: [OptArg] -> State QuerySettings [TermAttribute]
baseOptArgs = sequence . map toBase
  where toBase (k :== v) = TermAttribute k <$> runReQL (expr v)

-- | Build a term
op' :: Arr a => TermType -> a -> [OptArg] -> ReQL
op' t a b = ReQL $ do
  a' <- baseArray (arr a)
  b' <- baseOptArgs b
  case (t, a') of
    (FUNCALL, (Term FUNC [argsFunDatum, fun] [] : argsCall)) |
      Datum argsFunArray <- argsFunDatum,
      Just varsFun <- toDoubles argsFunArray,
      length varsFun == length argsCall,
      Just varsCall <- varsOf argsCall ->
        return $ alphaRename (zip varsFun varsCall) fun
    _ -> return $ Term t a' b'

-- | Build a term with no optargs
op :: Arr a => TermType -> a -> ReQL
op t a = op' t a []

toDoubles :: Datum -> Maybe [Double]
toDoubles (J.Array xs) = mapM toDouble $ toList xs
toDoubles _ = Nothing

toDouble :: Datum -> Maybe Double
toDouble (J.Number n) = Just $ toRealFloat n
toDouble _ = Nothing

varsOf :: [Term] -> Maybe [Double]
varsOf = sequence . map varOf
    
varOf :: Term -> Maybe Double
varOf (Term VAR [Datum d] []) = toDouble d
varOf _ = Nothing

alphaRename :: [(Double, Double)] -> Term -> Term
alphaRename assoc = fix $ \f x ->
  case varOf x of
    Just n
      | Just n' <- lookup n assoc ->
      Term VAR [Datum (toJSON n')] []
      | otherwise -> x
    _ -> updateChildren x f

updateChildren :: Term -> (Term -> Term) -> Term
updateChildren d@Datum{} _ = d
updateChildren (Term t a o) f = Term t (map f a) (map (mapTermAttribute f) o)

datumTerm :: J.ToJSON a => a -> ReQL
datumTerm = ReQL . return . Datum . toJSON

-- | A shortcut for inserting strings into ReQL expressions
-- Useful when OverloadedStrings makes the type ambiguous
str :: String -> ReQL
str = datumTerm

-- | A shortcut for inserting numbers into ReQL expressions
num :: Double -> ReQL
num = expr

instance Expr Int64 where
  expr = datumTerm

instance Expr Int where
  expr = datumTerm

instance Expr Integer where
  expr = datumTerm

instance Num ReQL where
  fromInteger = datumTerm
  a + b = op ADD (a, b)
  a * b = op MUL (a, b)
  a - b = op SUB (a, b)
  negate a = op SUB (0 :: Double, a)
  abs n = op BRANCH (op Term.LT (n, 0 :: Double), negate n, n)
  signum n = op BRANCH (op Term.LT (n, 0 :: Double),
                        -1 :: Double,
                        op BRANCH (op Term.EQ (n, 0 :: Double), 0 :: Double, 1 :: Double))
instance Expr T.Text where
  expr = datumTerm

instance Expr Bool where
  expr = datumTerm

instance Expr () where
  expr _ = ReQL $ return $ Datum J.Null

instance IsString ReQL where
  fromString = datumTerm

instance (a ~ ReQL) => Expr (a -> ReQL) where
  expr f = ReQL $ do
    v <- newVarId
    runReQL $ op FUNC (toJSON [v], expr $ f (op VAR [v]))

instance (a ~ ReQL, b ~ ReQL) => Expr (a -> b -> ReQL) where
  expr f = ReQL $ do
    a <- newVarId
    b <- newVarId
    runReQL $ op FUNC (toJSON [a, b], expr $ f (op VAR [a]) (op VAR [b]))

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
  expr = datumTerm

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

buildTerm :: Term -> WireTerm
buildTerm (Datum a@J.Array{}) = WireTerm $ toJSON (toWire MAKE_ARRAY, a)
buildTerm (Datum json) = WireTerm json
buildTerm (Term type_ args optargs) =
  WireTerm $ toJSON (
    toWire type_,
    map (termJSON . buildTerm) args,
    buildAttributes optargs)

buildAttributes :: [TermAttribute] -> J.Value
buildAttributes ts = toJSON $ M.fromList $ map toPair ts
 where toPair (TermAttribute a b) = (a, termJSON $ buildTerm b)

newtype WireQuery = WireQuery { queryJSON :: Value }

buildQuery :: ReQL -> Int64 -> Database -> [(T.Text, Value)] -> (WireQuery, Term)
buildQuery reql token db opts =
  (WireQuery $ toJSON (toWire START, termJSON pterm, J.object opts), bterm)
  where
    bterm = fst $ runState (runReQL reql) (def {queryToken = token,
                                                queryDefaultDatabase = db })
    pterm = buildTerm bterm

instance Show ReQL where
  show t = show . snd $ buildQuery t 0 (Database "") []

reqlToJSON :: ReQL -> Value
reqlToJSON t = queryJSON $ fst $ buildQuery t 0 (Database "") []

type Backtrace = [Frame]

data Frame = FramePos Int64 | FrameOpt T.Text

instance Show Frame where
    show (FramePos n) = show n
    show (FrameOpt k) = show k

instance J.FromJSON Frame where
  parseJSON (J.Number n) | Just i <- toBoundedInteger n = return $ FramePos i
  parseJSON (J.String s) = return $ FrameOpt s
  parseJSON _ = mempty

newtype WireBacktrace = WireBacktrace { backtraceJSON :: Value }

convertBacktrace :: WireBacktrace -> Backtrace
convertBacktrace (WireBacktrace b) =
  case J.fromJSON b of
    J.Success a -> a
    J.Error _ -> []

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

instance Expr Term where
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
