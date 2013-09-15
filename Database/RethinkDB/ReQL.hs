{-# LANGUAGE ExistentialQuantification, RecordWildCards, ScopedTypeVariables,
             FlexibleInstances, OverloadedStrings #-}

-- | Building RQL queries in Haskell
module Database.RethinkDB.ReQL (
  ReQL(..),
  op,
  BaseReQL(..),
  BaseAttribute(..),
  buildQuery,
  BaseArray(..),
  Backtrace, convertBacktrace,
  Expr(..),
  QuerySettings(..),
  newVar,
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
  canReturnVals,
  ) where

import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as M
import Data.Maybe (fromMaybe, catMaybes)
import Data.String (IsString, fromString)
import Data.List (intersperse)
import qualified Data.Sequence as S
import Control.Monad.State (State, get, put, runState)
import Control.Applicative ((<$>))
import Data.Default (Default, def)
import qualified Data.Text as T
import qualified Data.Aeson as J
import Data.Foldable (toList)

import Text.ProtocolBuffers hiding (Key, cons, Default)
import Text.ProtocolBuffers.Basic hiding (Default)

import Database.RethinkDB.Protobuf.Ql2.Term (Term(datum, args, optargs))
import qualified Database.RethinkDB.Protobuf.Ql2.Term as Term (Term(type'))
import Database.RethinkDB.Protobuf.Ql2.Term.TermType as TermType
  (TermType(
      MAKE_ARRAY, MAKE_OBJ, DATUM, GET, ADD, MUL, BRANCH,
      LT, EQ, FUNC, VAR, TABLE, DB, FUNCALL, ERROR))
import Database.RethinkDB.Protobuf.Ql2.Term.AssocPair (AssocPair(AssocPair))
import qualified Database.RethinkDB.Protobuf.Ql2.Query as Query (Query(type',query))
import Database.RethinkDB.Protobuf.Ql2.Query (Query(query))
import Database.RethinkDB.Protobuf.Ql2.Query.QueryType (QueryType(START))
import qualified Database.RethinkDB.Protobuf.Ql2.Datum as Datum (Datum(type'))
import Database.RethinkDB.Protobuf.Ql2.Datum (r_str, r_num, r_bool, r_array, r_object)
import qualified Database.RethinkDB.Protobuf.Ql2.Backtrace as QL (Backtrace, frames)
import qualified Database.RethinkDB.Protobuf.Ql2.Frame as QL (Frame(type', pos, Frame, opt))
import Database.RethinkDB.Protobuf.Ql2.Datum.DatumType
  (DatumType(R_NUM, R_BOOL, R_STR, R_ARRAY, R_OBJECT, R_NULL))
import Database.RethinkDB.Protobuf.Ql2.Frame.FrameType as QL (FrameType(POS, OPT))

import Database.RethinkDB.Objects as O

-- | An RQL term
data ReQL = ReQL { baseReQL :: State QuerySettings BaseReQL }

data BaseReQL = BaseReQL {
    termType :: TermType,
    termDatum :: Maybe Datum.Datum,
    termArgs :: BaseArray,
    termOptArgs :: [BaseAttribute] }

data QuerySettings = QuerySettings {
  queryToken :: Int64,
  queryDefaultDatabase :: Database,
  queryVarIndex :: Int,
  queryUseOutdated :: Maybe Bool,
  queryReturnVals :: Maybe Bool
  }

instance Default QuerySettings where
  def = QuerySettings 0 (Database "") 0 Nothing Nothing

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

canReturnVals :: ReQL -> ReQL
canReturnVals (ReQL t) = ReQL $ t >>= \ret -> do
  qs <- get
  case queryReturnVals qs of
    Nothing -> return ret
    Just rv -> return ret{
      termOptArgs = BaseAttribute "return_vals" (baseBool rv) : termOptArgs ret }

baseBool :: Bool -> BaseReQL
baseBool b = BaseReQL DATUM (Just defaultValue{
                                Datum.type' = Just R_BOOL, r_bool = Just b }) [] []

newVar :: State QuerySettings ReQL
newVar = do
  QuerySettings {..} <- get
  let n = queryVarIndex + 1
  put QuerySettings {queryVarIndex = n, ..}
  return $ expr n

instance Show BaseReQL where
  show (BaseReQL DATUM (Just dat) _ _) = showD dat
  show (BaseReQL fun _ args optargs) =
    show fun ++ " (" ++ concat (intersperse ", " (map show args ++ map show optargs)) ++ ")"

showD :: Datum.Datum -> String
showD d = case Datum.type' d of
  Just R_NUM -> show' $ r_num d
  Just R_BOOL -> show' $ r_bool d
  Just R_STR -> show' $ r_str d
  Just R_ARRAY -> show $ r_array d
  Just R_OBJECT -> show $ r_object d
  Just R_NULL -> "null"
  Nothing -> "Nothing"
  where show' Nothing = "Nothing"
        show' (Just a) = show a

-- | Convert other types to terms
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

-- | A list of String/Expr pairs
data Object = Object { baseObject :: State QuerySettings [BaseAttribute] }

data Attribute = forall e . (Expr e) => T.Text := e

data BaseAttribute = BaseAttribute T.Text BaseReQL

instance Show BaseAttribute where
  show (BaseAttribute a b) = T.unpack a ++ ": " ++ show b

class Obj o where
  obj :: o -> Object

instance Obj [Attribute] where
  obj = Object . mapM base
    where base (k := e) = BaseAttribute k <$> baseReQL (expr e)

instance Obj Object where
  obj = id

instance Obj () where
  obj _ = Object $ return []

-- | Build a term
op :: (Arr a, Obj o) => TermType -> a -> o -> ReQL
op t a b = ReQL $ do
  a' <- baseArray (arr a)
  b' <- baseObject (obj b)
  return $ BaseReQL t Nothing a' b'

datumTerm :: DatumType -> Datum.Datum -> ReQL
datumTerm t d = ReQL $ return $ BaseReQL DATUM (Just d { Datum.type' = Just t }) [] []

str :: String -> ReQL
str s = datumTerm R_STR defaultValue { r_str = Just (uFromString s) }

num :: Double -> ReQL
num = expr

instance Expr Int64 where
  expr i = datumTerm R_NUM defaultValue { r_num = Just (fromIntegral i) }

instance Expr Int where
  expr i = datumTerm R_NUM defaultValue { r_num = Just (fromIntegral i) }

instance Num ReQL where
  fromInteger x = datumTerm R_NUM defaultValue { r_num = Just (fromInteger x) }
  a + b = op ADD (a, b) ()
  a * b = op MUL (a, b) ()
  abs n = op BRANCH (op TermType.LT (n, 0 :: Double) (), negate n, n) ()
  signum n = op BRANCH (op TermType.LT (n, 0 :: Double) (),
                        -1 :: Double,
                        op BRANCH (op TermType.EQ (n, 0 :: Double) (), 0 :: Double, 1 :: Double) ()) ()

instance Expr T.Text where
  expr t = datumTerm R_STR defaultValue { r_str = Just (uFromString $ T.unpack t) }

instance Expr Bool where
  expr b = datumTerm R_BOOL defaultValue { r_bool = Just b }

instance Expr () where
  expr _ = datumTerm R_NULL defaultValue

instance IsString ReQL where
  fromString s = datumTerm R_STR defaultValue { r_str = Just (uFromString $ s) }

instance Expr (ReQL -> ReQL) where
  expr f = ReQL $ do
    v <- newVar
    baseReQL $ op FUNC ([v], f (op VAR [v] ())) ()

instance Expr (ReQL -> ReQL -> ReQL) where
  expr f = ReQL $ do
    a <- newVar
    b <- newVar
    baseReQL $ op FUNC ([a, b], f (op VAR [a] ()) (op VAR [b] ())) ()

instance Expr Table where
  expr (Table mdb name _) = withQuerySettings $ \QuerySettings {..} ->
    op TABLE (fromMaybe queryDefaultDatabase mdb, name) $ catMaybes [
      fmap ("use_outdated" :=) queryUseOutdated ]

instance Expr Database where
  expr (Database name) = op DB [name] ()

instance Expr J.Value where
  expr J.Null = expr ()
  expr (J.Bool b) = expr b
  expr (J.Number n) = expr (fromRational (toRational n) :: Double)
  expr (J.String t) = expr t
  expr (J.Array a) = expr a
  expr (J.Object o) = expr o

instance Expr Double where
  expr d = datumTerm R_NUM defaultValue { r_num = Just d }

instance Expr x => Expr (V.Vector x) where
  expr v = expr (V.toList v)

instance Expr a => Expr [a] where
  expr a = expr $ arr a

instance Expr Array where
  expr a = op MAKE_ARRAY a ()

instance Expr e => Expr (M.HashMap T.Text e) where
  expr m = expr $ obj $ map (uncurry (:=)) $ M.toList m

instance Expr Object where
  expr o = op MAKE_OBJ () o

buildTerm :: ReQL -> State QuerySettings Term
buildTerm = fmap buildBaseReQL . baseReQL

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

termToProtobuf :: ReQL -> Query.Query
termToProtobuf t = fst $ buildQuery t 0 (Database "")

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