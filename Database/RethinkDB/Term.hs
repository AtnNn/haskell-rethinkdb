{-# LANGUAGE KindSignatures, DataKinds, ExistentialQuantification, GADTs,
             TypeFamilies, TypeOperators, PolyKinds, ConstraintKinds, RankNTypes,
             MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             FlexibleContexts, RecordWildCards, GeneralizedNewtypeDeriving #-}

-- | Building RQL queries in Haskell
module Database.RethinkDB.Term where

import Data.List
import qualified Data.Sequence as S
import Control.Monad.State
import GHC.Prim
import Control.Applicative
import Data.Default

import Text.ProtocolBuffers hiding (Key, cons, Default)
import Text.ProtocolBuffers.Basic hiding (Default)

import Database.RethinkDB.Protobuf.Ql2.Term2 as Term2
import Database.RethinkDB.Protobuf.Ql2.Term2.TermType
import Database.RethinkDB.Protobuf.Ql2.Term2.AssocPair
import Database.RethinkDB.Protobuf.Ql2.Query2 as Query2
import Database.RethinkDB.Protobuf.Ql2.Query2.QueryType
import Database.RethinkDB.Protobuf.Ql2.Datum as Datum
import Database.RethinkDB.Protobuf.Ql2.Datum.DatumType

import qualified Database.RethinkDB.Type as T
import Database.RethinkDB.Term.Undecidable
import Database.RethinkDB.Crud


-- | An RQL term
data Term (t :: T.Type) = Term { baseTerm :: State QuerySettings BaseTerm }

data BaseTerm = BaseTerm {
    termType :: TermType,
    termDatum :: Maybe Datum,
    termArgs :: BaseArray,
    termOptArgs :: [BaseAttribute]
    }

data QuerySettings = QuerySettings {
  queryToken :: Int64,
  queryDefaultDatabase :: Database,
  queryVarIndex :: Int,
  queryUseOutdated :: Bool
  }

instance Default QuerySettings where
  def = QuerySettings 0 (Database "") 0 False

instance Show BaseTerm where
  show (BaseTerm DATUM (Just dat) _ _) = showD dat
  show (BaseTerm fun _ args optargs) =
    show fun ++ " (" ++ concat (intersperse ", " (mapA show args ++ map show optargs)) ++ ")"

showD :: Datum -> String
showD d = case Datum.type' d of
  R_NUM -> show' $ r_num d
  R_BOOL -> show' $ r_bool d
  R_STR -> show' $ r_str d
  R_ARRAY -> show $ r_array d
  R_OBJECT -> show $ r_object d
  where show' Nothing = "Nothing"
        show' (Just a) = show a

-- | Convert other types to terms
class Expr e where
  type ExprType e :: T.Type
  expr :: e -> Term (ExprType e)

instance Expr (Term t) where
  type (ExprType (Term t)) = t
  expr t = t

-- | A list of terms
data Array = Array { baseArray :: State QuerySettings BaseArray }

data BaseArray = Nil | Cons BaseTerm BaseArray

-- | Build arrays of exprs
class Arr a where
  arr :: a -> Array

cons :: Expr e => e -> Array -> Array
cons x xs = Array $ do
  let Term t = expr x
  bt <- t
  let Array a = xs
  xs' <- a
  return $ Cons bt xs'

instance Arr () where
  arr () = Array $ return Nil

instance Expr a => Arr [a] where
  arr [] = Array $ return Nil
  arr (x:xs) = cons x (arr xs)

instance (Expr a, Expr b) => Arr (a, b) where
  arr (a,b) = cons a $ cons b $ arr ()

instance (Expr a, Expr b, Expr c) => Arr (a, b, c) where
  arr (a,b,c) = cons a $ cons b $ cons c $ arr ()

instance (Expr a, Expr b, Expr c, Expr d) => Arr (a, b, c, d) where
  arr (a,b,c,d) = cons a $ cons b $ cons c $ cons d $ arr ()

-- | A list of String/Expr pairs
data Object = Object { baseObject :: State QuerySettings [BaseAttribute] }

type Key = String

data Attribute = forall e . (Expr e) => Key := e

data BaseAttribute = BaseAttribute Key BaseTerm

instance Show BaseAttribute where
  show (BaseAttribute a b) = a ++ ": " ++ show b

mapA :: (BaseTerm -> a) -> BaseArray -> [a]
mapA f Nil = []
mapA f (Cons x xs) = f x : mapA f xs

-- | Build an Object
obj :: [Attribute] -> Object
obj = Object . mapM base
      where base (k := e) = BaseAttribute k <$> baseTerm (expr e)

-- | Build a term
op :: Arr a => TermType -> a -> [Attribute] -> Term c
op t a b = Term $ do
  a' <- baseArray (arr a)
  b' <- baseObject (obj b)
  return $ BaseTerm t Nothing a' b'

type a ~~~ b = (Expr a, T.Instance b (ExprType a))

type a ~~ b = (Expr a, DatumOrInstance b (ExprType a))

instance Num (Term T.Number) where
  fromInteger x = Term $ return $ BaseTerm DATUM (Just $ defaultValue {
    Datum.type' = R_NUM, r_num = Just (fromInteger x) }) Nil []
  a + b = op ADD (a, b) []
  a * b = op MUL (a, b) []
  -- TODO

buildTerm :: Term t -> State QuerySettings Term2
buildTerm = fmap buildBaseTerm . baseTerm

buildBaseTerm :: BaseTerm -> Term2
buildBaseTerm BaseTerm {..} = defaultValue {
    Term2.type' = termType,
    datum = termDatum,
    args = buildBaseArray termArgs,
    optargs = buildTermAssoc termOptArgs }

buildBaseArray :: BaseArray -> Seq Term2
buildBaseArray Nil = S.empty
buildBaseArray (Cons x xs) = buildBaseTerm x S.<| buildBaseArray xs

buildTermAssoc :: [BaseAttribute] -> Seq AssocPair
buildTermAssoc = S.fromList . map buildTermAttribute

buildTermAttribute :: BaseAttribute -> AssocPair
buildTermAttribute (BaseAttribute k v) = AssocPair (uFromString k) (buildBaseTerm v)

buildQuery :: Term t -> Int64 -> Database -> Query2
buildQuery term token db = defaultValue {
  Query2.type' = START,
  query = Just $ fst $ runState (buildTerm term) (def {queryToken = token,
                                                       queryDefaultDatabase = db }) }
